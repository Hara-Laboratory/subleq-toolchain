module Language.Subleq.Assembly.Printer where

import Language.Subleq.Assembly.Prim
import Text.PrettyPrint
import qualified Data.Map as M

printId :: Id -> Doc
printId = text

printLoc :: Id -> Doc
printLoc = text . (++ ":")

printExpr :: Expr -> Doc
printExpr (Identifier i) = printId i
printExpr (Number n) = integer n
printExpr (EAdd e1 e2) = parens $ sep [text "+", printExpr e1, printExpr e2]
printExpr (ESub e1 e2) = parens $ sep [text "-", printExpr e1, printExpr e2]

printLocExpr :: LocExpr -> Doc
printLocExpr (l, e) = maybe empty printLoc l <> printExpr e

printInstruction :: Instruction -> Doc
printInstruction Subleq = empty

printElement :: Element -> Doc
printElement (ElemInst i es) = printInstruction i <+> hsep (map printLocExpr es) <> semi
printElement (SubroutineCall l x es) = maybe empty printLoc l <> text "$(@@" <> printId x <+> hsep (punctuate comma (map printExpr es)) <> text ")" <> semi
printElement (ElemLoc l) = printLoc l

printElements :: [Element] -> Doc
printElements = vcat . map printElement

printObject :: Object -> Doc
printObject (Subroutine n args elems) = text ("@" ++ n) <+> hsep (punctuate comma (map printId args)) $$ nest 4 (printElements elems)
printObject (Macro n args elems) = text ("@@" ++ n) <+> hsep (punctuate comma (map printId args)) $$ nest 4 (printElements elems)

printModule :: Module -> Doc
printModule (Module m) = fsep . map printObject . M.elems $ m

