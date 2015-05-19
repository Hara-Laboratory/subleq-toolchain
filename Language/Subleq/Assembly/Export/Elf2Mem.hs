module Language.Subleq.Assembly.Export.Elf2Mem(renderLocatePackResult, renderLoadPackResult) where

import qualified Language.Subleq.Assembly as A
import Data.List
import Data.Function
import qualified Data.Map as M
import Text.PrettyPrint

collect :: (Num a, Eq a, Ord a)=> [(a, b)] -> [(a, [b])]
collect = collect' Nothing . sortBy (compare `on` fst)
  where
    collect' Nothing                 []                              = []
    collect' (Just (a,  _, vs))      []                              = [(a, reverse vs)]
    collect' Nothing                 ((a,v):avs)                     = collect' (Just (a, a, [v])) avs
    collect' (Just (a, a', vs))      x@((a'',v):avs) | a' + 1 == a'' = collect' (Just (a, a'', v:vs)) avs
                                                     | otherwise     = (a, reverse vs) : collect' Nothing x

docMemory :: M.Map Integer Integer -> Doc
docMemory m = vcat $ map docBlick l
  where
    l = collect $ M.toAscList m
    docBlick (addr, vals) = text "@" <> integer addr <> colon <+> hsep (map integer vals)

renderLoadPackResult :: (Integer, M.Map A.Id Integer, M.Map Integer Integer) -> String
renderLoadPackResult (end, funcs, mem) = render $ vcat [endAddr, text "", addrTable, text "", memCont]
  where
    endAddr = (text "[header]" $+$) . nest 4 . vcat $ headers ++ [text "end" <> colon <+> integer end]
    addrTable = (text "[symbols]" $+$) . nest 4 . vcat $ map (\(func, addr) -> text func <> colon <+> text "@" <> integer addr ) $ M.toList funcs
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
    containts = vcat $ map (\(addr, obj) -> text "Address" <+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
    endAddr :: Doc
    endAddr = text "End Address" <> colon <+> integer end

