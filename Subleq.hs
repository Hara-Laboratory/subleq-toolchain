{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveDataTypeable, TemplateHaskell #-}
module Main where

-- import Language.Subleq.Model.Prim
import Language.Subleq.Model.Memory as Mem
-- import Language.Subleq.Model.Architecture.IntMachine
import qualified Language.Subleq.Model.InstructionSet.Subleq as Subleq
import qualified Language.Subleq.Assembly as A
import Text.Parsec
import Control.Applicative
import Text.PrettyPrint
import qualified Text.PrettyPrint as PP
import Text.Printf
import Data.List
import Data.Function
-- import Data.Map (Map)
import qualified Data.Map as M
-- import Control.Monad.State
import Control.Lens
import System.Console.CmdArgs

locateArg :: A.LocateArg
locateArg xs = M.fromList $ zip xs [38, 39, 37] -- DEST_LOC, SRC1_LOC, SRC2_LOC 

subleqMA :: A.MemoryArchitecture (M.Map Integer Integer)
subleqMA = A.MemoryArchitecture { A.instructionLength = 3
                                , A.wordLength = 1
                                , A.locateArg = A.locateArgDefault
                                , A.locateStatic = M.fromList [ ("Lo", 32)
                                                              , ("Hi", 33)
                                                              , ("End", -0x1)
                                                              , ("Z",  36)
                                                              , ("T0", 40)
                                                              , ("T1", 41)
                                                              , ("T2", 42)
                                                              , ("T3", 43)
                                                              , ("T4", 44)
                                                              , ("T5", 45)
                                                              , ("T6", 46)
                                                              , ("CW", 47)
                                                              , ("Inc", 48)
                                                              , ("Dec", 49)
                                                              ]
                                , A.writeWord = Mem.write
                                }

inc, dec :: (Num a) => a
inc = 0x4
dec = 0x5

wordLength :: (Num a) => a
wordLength = 32

subleqMAInitialMem :: (Num a, Ord a) => M.Map a a
subleqMAInitialMem = Mem.write 0xf wordLength . Mem.write inc (-1) . Mem.write dec 1 $ M.empty


-- main :: IO ()
-- main = (unlines . take 50 . map showIntSubleqState <$> testMult 1 3) >>= putStrLn

data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)

data Architecture = SubleqInt
            deriving (Show, Data, Typeable)

data Subleq = Subleq { _file :: FilePath
                     , _out :: FilePath
                     , _arch :: String
                     , _format :: String
                     , _startAddress :: Integer
                     }
            deriving (Show, Data, Typeable)
makeLenses ''Subleq

sample :: Subleq
sample = Subleq { _file = def &= argPos 0 &= typFile
                , _out = def &= explicit &= name "o" &= name "out" &= typFile &= help "Output file"
                , _format = def &= explicit &= name "f" &= name "format" &= typ "FORMAT" &= help "Output format (id, expand, packed, elf2mem)"
                , _arch = def &= explicit &= name "m" &= name "target" &= typ "TARGET" &= opt "subleq-int" &= help "Target architecture (subleq-int)"
                , _startAddress = def &= explicit &= name "b" &= name "begin" &= typ "ADDRESS" &= opt "100" &= help "The address where the subleq routines start."
                }
         &= help "Assemble subleq programs."
         &= summary "Subleq Assembler v0.1.1.4 (C) SAKAMOTO Noriaki"

main :: IO ()
main = do
    s <- cmdArgs sample
    print s
    assemble s

renderLocatePackResult :: (Integer, M.Map a (Integer, A.Object)) -> String
renderLocatePackResult (end, ma) = render $ vcat [endAddr, containts]
  where
    containts :: Doc
    containts = vcat $ map (\(addr, obj) -> text "Address" <+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
    endAddr :: Doc
    endAddr = text "End Address" <> colon <+> integer end

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

assemble :: Subleq -> IO ()
assemble s = do
    mo <- either (error . show) id . parse A.parseModule "parserModule" <$> readFile (s^.file)
    writeFile (s^.out) $ convert (s^.format) mo 
  where
    expand =  A.expandMacroAll
    renderModule = render . A.printModule
    convert "id" = renderModule
    convert "expand" = renderModule . expand
    convert "packed" = \mo-> renderLoadPackResult $ A.loadModulePacked subleqMA (s^.startAddress) (expand mo) M.empty
    convert "elf2mem" = convert "packed"
    convert fmt = error $ printf "Unknown format: `%s'" fmt

    -- let (end, ma) = A.loadModulePacked subleqMA 100 mo
    -- putStrLn $ render $ vcat $ map (\(addr, obj) -> text "Address" <+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
