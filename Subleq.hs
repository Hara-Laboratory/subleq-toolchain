{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveDataTypeable, TemplateHaskell #-}
module Main where

-- import Language.Subleq.Model.Prim
import qualified Language.Subleq.Model.Memory as Mem
-- import Language.Subleq.Model.Architecture.IntMachine
import qualified Language.Subleq.Model.InstructionSet.Subleq as Subleq
import qualified Language.Subleq.Assembly as A
import Text.Parsec
import Control.Applicative
import Text.PrettyPrint
import qualified Text.PrettyPrint as PP
import Text.Printf
import Data.Maybe
import Data.List
import Data.Function
-- import Data.Map (Map)
import qualified Data.Map as M
-- import Control.Monad.State
import Control.Lens
import System.Console.CmdArgs

locateArg :: A.LocateArg
-- locateArg xs = M.fromList $ zip xs [37, 38, 39] -- DEST_LOC, SRC1_LOC, SRC2_LOC
locateArg xs = M.fromList $ zip xs [0..] -- SRC1_LOC, SRC2_LOC, DEST_LOC

subleqMA :: A.MemoryArchitecture (M.Map Integer Integer)
subleqMA = A.MemoryArchitecture { A.instructionLength = 3
                                , A.wordLength = 1
                                , A.locateArg = locateArg
                                , A.locateStatic = M.fromList [ ("Lo", 32)
                                                              , ("Hi", 33)
                                                              , ("End", exitPoint)
                                                              , ("Z",  23)
                                                              , ("T0", 16)
                                                              , ("T1", 17)
                                                              , ("T2", 18)
                                                              , ("T3", 19)
                                                              , ("T4", 20)
                                                              , ("T5", 21)
                                                              , ("T6", 22)
                                                              , ("CW", cw)
                                                              , ("Inc", inc)
                                                              , ("Dec", dec)
                                                              ]
                                , A.writeWord = Mem.write
                                }

cw, inc, dec, exitPoint :: (Num a) => a
cw  = 26
inc = 24
dec = 25
exitPoint = -1

wordLength :: (Num a) => a
wordLength = 32

subleqMAInitialMem :: (Num a, Ord a) => M.Map a a
subleqMAInitialMem = Mem.write cw wordLength . Mem.write inc (-1) . Mem.write dec 1 $ M.empty

data LocationMethod = SequenceFrom Int
                    | UseFrom [Int]
    deriving (Read, Show, Data, Typeable)

data SubleqConfig = SubleqConfig { _instructionLength :: Integer
                                 , _wordLengthInAddressingUnit :: Integer
                                 , _wordLengthInBit :: Integer
                                 , _argumentLocations :: LocationMethod
                                 , _staticLocations :: [(A.Id, Integer)]
                                 }
            deriving (Read, Show, Data, Typeable)
makeLenses ''SubleqConfig

defaultConfig = SubleqConfig { _instructionLength = 3
                             , _wordLengthInAddressingUnit = 1
                             , _wordLengthInBit = 32
                             , _argumentLocations = SequenceFrom 0
                             , _staticLocations = [ ("Lo", 32)
                                                  , ("Hi", 33)
                                                  , ("End", exitPoint)
                                                  , ("Z",  23)
                                                  , ("T0", 16)
                                                  , ("T1", 17)
                                                  , ("T2", 18)
                                                  , ("T3", 19)
                                                  , ("T4", 20)
                                                  , ("T5", 21)
                                                  , ("T6", 22)
                                                  , ("CW", cw)
                                                  , ("Inc", inc)
                                                  , ("Dec", dec)
                                                  ]
                             }

cases2015Config = SubleqConfig { _instructionLength = 3
                             , _wordLengthInAddressingUnit = 1
                             , _wordLengthInBit = 32
                             , _argumentLocations = UseFrom [37, 38, 39]
                             , _staticLocations = [ ("Lo", 32)
                                                  , ("Hi", 33)
                                                  , ("End", 999)
                                                  , ("Z",  36)
                                                  , ("T0", 40)
                                                  , ("T1", 41)
                                                  , ("T2", 42)
                                                  , ("T3", 43)
                                                  , ("T4", 44)
                                                  , ("T5", 45)
                                                  , ("T6", 46)
                                                  , ("CW", cw)
                                                  , ("Inc", inc)
                                                  , ("Dec", dec)
                                                  ]
                             }
memoryArchitectureFromConfig :: SubleqConfig -> A.MemoryArchitecture (M.Map Integer Integer)
memoryArchitectureFromConfig c = A.MemoryArchitecture { A.instructionLength = c^.instructionLength
                                                      , A.wordLength = c^.wordLengthInAddressingUnit
                                                      , A.locateArg = locateArg' . map fromIntegral . useAddrs $ c^.argumentLocations
                                                      , A.locateStatic = M.fromList $ c^.staticLocations
                                                      , A.writeWord = Mem.write
                                                      }
    where
      useAddrs (SequenceFrom n) = [n..]
      useAddrs (UseFrom ns) = ns
      locateArg' as xs = M.fromList $ zip xs as

initialValues :: (Num a, Integral a) => a -> M.Map String a
initialValues wl = M.fromList [ ("Inc", -1)
                              , ("Dec",  1)
                              , ("CW", wl)
                              , ("Min", - (2^wl))
                              , ("Max", 2^wl - 1)
                              ]

initialMemFromConfigure :: (Num a, Ord a) => SubleqConfig -> M.Map a a
initialMemFromConfigure c = foldr (uncurry Mem.write) M.empty ls
    where
      ls = mapMaybe f sls
      sls = mapped._2 %~ fromIntegral $ (c^.staticLocations)   
      f :: (Num a)=>(A.Id, Integer) -> Maybe (a, a)
      f (i, l) = do v <- i `M.lookup` initialValues (c^.wordLengthInBit)
                    return (fromIntegral l, fromIntegral v)

-- main :: IO ()
-- main = (unlines . take 50 . map showIntSubleqState <$> testMult 1 3) >>= putStrLn

data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)

data Architecture = SubleqInt
            deriving (Show, Data, Typeable)

data Subleq = Subleq { _file :: FilePath
                     , _out :: FilePath
                     , _arch :: String
                     , _format :: String
                     , _config :: String
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
                , _config = def &= explicit &= name "c" &= name "config" &= typ "CONFIG" &= opt (show defaultConfig) &= help "Detailed configure for subleq architecture."
                }
         &= help "Assemble subleq programs"
         &= details ["default config:", show defaultConfig]
         &= summary "Subleq Assembler v0.1.7.0 (C) SAKAMOTO Noriaki"

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
    convert "packed" = \mo-> renderLoadPackResult $ A.loadModulePacked (memoryArchitectureFromConfig cfg) (s^.startAddress) (expand mo) (initialMemFromConfigure cfg)
    convert "elf2mem" = convert "packed"
    convert fmt = error $ printf "Unknown format: `%s'" fmt
    cfg = read $ s^.config

    -- let (end, ma) = A.loadModulePacked subleqMA 100 mo
    -- putStrLn $ render $ vcat $ map (\(addr, obj) -> text "Address" <+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
