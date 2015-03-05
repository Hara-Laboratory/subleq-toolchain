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
import Text.Printf
-- import Data.Maybe
-- import Data.Map (Map)
import qualified Data.Map as M
-- import Control.Monad.State
import Control.Lens
import System.Console.CmdArgs

subleqMA :: A.MemoryArchitecture (M.Map Integer Integer)
subleqMA = A.MemoryArchitecture { A.instructionLength = 3
                                , A.wordLength = 1
                                , A.locateArg = A.locateArgDefault
                                , A.locateStatic = M.fromList [ ("Lo", 0x120)
                                                              , ("End", -0x1)
                                                              , ("Inc", 0x4)
                                                              , ("Dec", 0x5)
                                                              , ("Z", 0x6)
                                                              , ("T0", 0x8)
                                                              , ("T1", 0x9)
                                                              , ("T2", 0xa)
                                                              , ("T3", 0xb)
                                                              , ("T4", 0xc)
                                                              ]
                                , A.writeWord = Mem.write
                                }

parseFile :: FilePath -> IO (Either ParseError A.Module)
parseFile path = parse A.parseModule "parserModule" <$> readFile path

-- main :: IO ()
-- main = (unlines . take 50 . map showIntSubleqState <$> testMult 1 3) >>= putStrLn

data Sample = Sample {hello :: String} deriving (Show, Data, Typeable)

data Architecture = SubleqInt
            deriving (Show, Data, Typeable)

data Subleq = Subleq { _file :: FilePath
                     , _out :: FilePath
                     , _arch :: String
                     , _format :: String
                     }
            deriving (Show, Data, Typeable)
makeLenses ''Subleq

sample :: Subleq
sample = Subleq { _file = def &= argPos 0 &= typFile
                , _out = def &= explicit &= name "o" &= name "out" &= typFile &= help "Output file"
                , _format = def &= explicit &= name "f" &= name "format" &= typ "FORMAT" &= help "Output format (id, expand, packed, elf2mem)"
                , _arch = def &= explicit &= name "m" &= name "target" &= typ "TARGET" &= opt "subleq-int" &= help "Target architecture (subleq-int)"
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

renderLoadPackResult :: (Integer, M.Map A.Id Integer, M.Map Integer Integer) -> String
renderLoadPackResult (end, funcs, mem) = undefined
  where
    addrTable = vcat $ map (\(func, addr) -> text func <> colon <+> integer addr ) $ M.toList funcs

assemble :: Subleq -> IO ()
assemble s = do
    mo <- either (error . show) id . parse A.parseModule "parserModule" <$> readFile (s^.file)
    writeFile (s^.out) $ convert (s^.format) mo 
  where
    expand =  A.expandMacroAll
    renderModule = render . A.printModule
    convert "id" = renderModule
    convert "expand" = renderModule . expand
    convert "packed" = \mo-> renderLoadPackResult $ A.loadModulePacked subleqMA 100 (expand mo) M.empty
    convert fmt = error $ printf "Unknown format: `%s'" fmt

    -- let (end, ma) = A.loadModulePacked subleqMA 100 mo
    -- putStrLn $ render $ vcat $ map (\(addr, obj) -> text "Address" <+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
