module Language.Subleq.Assembly.Export.Elf2Mem(renderLocatePackResult, renderLoadPackResult) where

import qualified Language.Subleq.Assembly as A
import Data.List
import Data.Function
import qualified Data.Map as M
import Text.PrettyPrint hiding ((<+>))
import qualified Text.PrettyPrint as PP
import Control.Arrow

collect :: (Num a, Eq a, Ord a)=> [(a, b)] -> [(a, [b])]
collect = collect' Nothing . sortBy (compare `on` fst)
  where
    collect' Nothing                 []                              = []
    collect' (Just (a,  _, vs))      []                              = [(a, reverse vs)]
    collect' Nothing                 ((a,v):avs)                     = collect' (Just (a, a, [v])) avs
    collect' (Just (a, a', vs))      x@((a'',v):avs) | a' + 1 == a'' = collect' (Just (a, a'', v:vs)) avs
                                                     | otherwise     = (a, reverse vs) : collect' Nothing x

docMemory :: (Integral a, Integral w)=>M.Map a w -> Doc
docMemory m = vcat $ map docBlick l
  where
    l = collect . map (fromIntegral *** fromIntegral) $ M.toAscList m
    docBlick (addr, vals) = text "@" <> integer addr <> colon PP.<+> hsep (map integer vals)

renderLoadPackResult :: (Integral a, Integral w)=>(Integer, M.Map A.Id Integer, M.Map a w) -> String
renderLoadPackResult (end, funcs, mem) = render $ vcat [endAddr, text "", addrTable, text "", memCont]
  where
    endAddr = (text "[header]" $+$) . nest 4 . vcat $ headers ++ [text "end" <> colon PP.<+> integer end]
    addrTable = (text "[symbols]" $+$) . nest 4 . vcat $ map (\(func, addr) -> text func <> colon PP.<+> text "@" <> integer addr ) $ M.toList funcs
    memCont = (text "[text]" $+$) . nest 4 . docMemory $ mem
    headers = [ text "version: 1"
              , text "type: packed"
              , text "byte-order: big-endian"
              , text "word-size: 4"
              ]

renderLocatePackResult :: (Integer, M.Map a (Integer, A.Object)) -> String
renderLocatePackResult (end, ma) = render $ vcat [endAddr, containts]
  where
    containts :: Doc
    containts = vcat $ map (\(addr, obj) -> text "Address" PP.<+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
    endAddr :: Doc
    endAddr = text "End Address" <> colon PP.<+> integer end

