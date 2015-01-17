{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FlexibleContexts #-}
-- {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies, FlexibleInstances, FlexibleContexts #-}
module Subleq.Assembly.Parser where

-- import Data.Map (Map)
-- import qualified Data.Map as M
-- import Control.Monad.State
-- import Control.Lens
import Control.Applicative ((<$>),(<*),(*>))
import Control.Monad
import Text.Parsec

type Id = String
type Location = String

data Expr = Identifier Id
          | Number Integer
    deriving (Read, Show, Eq, Ord)

type LocExpr = (Maybe Location, Expr)
--    deriving (Read, Show, Eq, Ord)

data Instruction = Subleq
    deriving (Read, Show, Eq, Ord)

data Element = ElemInst Instruction [LocExpr]
             | SubroutineCall (Maybe Location) Id [Expr]
    deriving (Read, Show, Eq, Ord)

data Object = Subroutine Id [Id] [Element]
            | Macro Id [Id] [Element]
    deriving (Read, Show, Eq, Ord)

type Module = [Object]
--    deriving (Read, Show, Eq, Ord)

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
    cs <- many alphaNum
    return (c:cs)

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

parseExpr :: Stream b m Char => ParsecT b u m Expr
parseExpr = (Number <$> parseIntegerLiteral) <|> (Identifier <$> parseId)

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
    return $ ElemInst insn args

parseSubroutineCall :: Stream b m Char => ParsecT b u m Element
parseSubroutineCall = do
    loc <- optionMaybe parseLoc
    (n, args) <- between (string "$(@@") (string ")") content
    return $ SubroutineCall loc n args
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
    -- es <- many (parseElement <* spaces)
    return $ (if isMacro then Macro else Subroutine) n args es

parseModule :: Stream b m Char => ParsecT b u m Module
parseModule = many (parseObject <* spaces) <* eof

