{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Language.Subleq.Assembly.Parser where

import Language.Subleq.Assembly.Prim
import Control.Applicative ((<$>),(<*),(*>))
import Control.Monad
import Text.Parsec
import Text.Printf
-- import Data.Map (Map)
import qualified Data.Map as M
import Data.List

symbol :: Stream b m Char => String -> ParsecT b u m String
symbol s = nonLineBreakSpaces *> string s <* nonLineBreakSpaces

nonLineBreakSpace :: Stream b m Char => ParsecT b u m ()
nonLineBreakSpace = void $ oneOf " \t\v"

nonLineBreakSpaces :: Stream b m Char => ParsecT b u m ()
nonLineBreakSpaces = try . void $ many nonLineBreakSpace

parseIdChar :: Stream b m Char => ParsecT b u m Char
parseIdChar = oneOf "abc"

parseId :: Stream b m Char => ParsecT b u m Id
parseId = do
    c <- letter
    cs <- many (alphaNum <|> oneOf "_")
    return (c:cs)

parseExternalReference :: Stream b m Char => ParsecT b u m Expr
parseExternalReference = do
    string "&@"
    c <- lower
    cs <- many alphaNum
    return $ Identifier ('_':c:cs)

parseSubroutineName :: Stream b m Char => ParsecT b u m Id
parseSubroutineName = do
    c <- lower
    cs <- many alphaNum
    return (c:cs)

parseSubroutineArgument :: Stream b m Char => ParsecT b u m Id
parseSubroutineArgument = parseId
--    do
--    c <- letter -- char 'A'
--    cs <- many alphaNum
--    return (c:cs)

parseLoc :: Stream b m Char => ParsecT b u m Location
parseLoc = do
    c <- upper <|> lower
    cs <- many alphaNum <* char ':'
    return (c:cs)

parseIntegerLiteral :: Stream b m Char => ParsecT b u m Integer
parseIntegerLiteral = do
    s <- option 1 $ char '-' *> return (-1)
    n <- read <$> many1 digit
    return (s * n)

parseCurrentPos :: Stream b m Char => ParsecT b u m Expr
parseCurrentPos = do
    char '?'
    return $ Identifier "?"

parseExprParen :: Stream b m Char => ParsecT b u m Expr
parseExprParen = do
    char '(' <* spaces
    op <- oneOf "+-" <* space <* spaces
    e1 <- parseExpr
    e2 <- optionMaybe (space *> spaces *> parseExpr) <* spaces
    char ')'
    return $ op' op e1 e2
  where
    op' '+' e1 Nothing = EAdd (Number 0) e1
    op' '+' e1 (Just e2') = EAdd e1 e2'
    op' '-' e1 Nothing = ESub (Number 0) e1
    op' '-' e1 (Just e2') = ESub e1 e2'

parseExprCurrentPos :: Stream b m Char => ParsecT b u m Expr
parseExprCurrentPos = do
    char '(' <* spaces
    e1 <- parseCurrentPos
    op <- oneOf "+-" <* spaces
    e2 <- parseExpr <* spaces <* char ')'
    return $ op' op e1 e2
  where
    op' '+' = EAdd
    op' '-' = ESub

parseExpr :: Stream b m Char => ParsecT b u m Expr
parseExpr = parseExprParen <|> (Number <$> parseIntegerLiteral) <|> parseExternalReference <|> (Identifier <$> parseId)

parseLocExpr :: Stream b m Char => ParsecT b u m LocExpr
parseLocExpr = do
    loc <- optionMaybe $ try parseLoc
    expr <- parseExpr
    return (loc, expr)

parseInstructionType :: Stream b m Char => ParsecT b u m Instruction
parseInstructionType = (string "!subleq" *> return Subleq) <|> return Subleq

parseInstruction :: Stream b m Char => ParsecT b u m Element
parseInstruction = do
    insn <- parseInstructionType <* spaces
    args <- (parseLocExpr `sepBy` (space >> spaces)) <* symbol ";"
    let (arityMin, arityMax) = instructionArity insn
    let arity = length args
    if arityMin <= arity && arity <= arityMax
      then return $ ElemInst insn args
      else error $ printf "Instruction %s takes %d to %d arguments, but got: %s" (show insn) arityMin arityMax (show args)

parseSubroutineCall :: Stream b m Char => ParsecT b u m Element
parseSubroutineCall = do
    loc <- optionMaybe parseLoc
    (n, args) <- between (string "$(@@") (string ")" >> symbol ";") content
    return $ SubroutineCall loc ('@':n) args
  where
    content = do
      n <- parseSubroutineName <* space <* spaces
      args <- (parseExpr `sepBy` symbol ",") <* spaces
      return (n, args)

parseElement :: Stream b m Char => ParsecT b u m Element
parseElement = try parseSubroutineCall <|> parseInstruction

parseHeader :: Stream b m Char => ParsecT b u m (Id, [Id])
parseHeader = do
    n <- parseSubroutineName <* many1 nonLineBreakSpace
    args <- parseSubroutineArgument `sepBy` try (symbol ",") <* (try (nonLineBreakSpaces >> void parseComment) <|> void (many nonLineBreakSpace >> endOfLine))
    return (n, args)

parseComment :: Stream b m Char => ParsecT b u m String
parseComment = string "//"  >> manyTill anyChar (try endOfLine)

skipCommentOrSpaces :: Stream b m Char => ParsecT b u m ()
skipCommentOrSpaces = spaces >> (void (try parseComment) <|> spaces)

parseObject :: Stream b m Char => ParsecT b u m Object
parseObject = do
    isMacro <- try (string "@@" *> return True) <|> (string "@" *> return False)
    (n, args) <- parseHeader
    es <- many (parseElement <* many nonLineBreakSpace <* skipCommentOrSpaces)
    let obj = (if isMacro then makeMacro else makeSubroutine) n args es
    let errors = errorsObject obj
    -- es <- many (parseElement <* spaces)
    if null errors
    then return obj
    else error $ unlines errors
  where
    makeMacro n args es = Macro ('@': n) args es
    makeSubroutine n args es = Subroutine n args es

parseMeaninglessLine :: Stream b m Char => ParsecT b u m String
parseMeaninglessLine = (replicate 1 <$> endOfLine) <|> parseComment

parseModule :: Stream b m Char => ParsecT b u m Module
parseModule = do
    many parseMeaninglessLine
    objs <- many (parseObject <* spaces) <* eof
    let freqs = M.fromListWith (+) [(objectId obj, 1) | obj <- objs]
    if M.null (M.filter (> (1 :: Integer)) freqs)
      then return $ Module $ M.fromList [(objectId obj, obj) | obj <- objs]
      else fail $ "Multiple definitions: " ++ intercalate ", " (M.keys $ M.filter (> 1) freqs)

