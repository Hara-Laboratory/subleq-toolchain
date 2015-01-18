{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import Subleq.Model.Prim
import Subleq.Model.Memory as Mem
import Subleq.Model.Architecture.IntMachine
import qualified Subleq.Model.InstructionSet.Subleq as Subleq
import qualified Subleq.Assembly as A
import Text.Parsec
import Control.Applicative
import Text.PrettyPrint
import Text.Printf
import Data.Maybe
-- import Data.Map (Map)
import qualified Data.Map as M
-- import Control.Monad.State
-- import Control.Lens

testSubleq :: [IntSubleqState]
testSubleq = runMachineHist Subleq.step Subleq.initialMachine

testParser :: IO (Either ParseError A.Module)
testParser = parse A.parseModule "parserModule" <$> readFile "test.sq"

testMacro :: IO A.Module
testMacro = either (error . show) A.expandMacroAll <$> testParser

testPrint :: IO ()
testPrint = do
    m <- testMacro
    putStrLn $ render $ A.printModule m

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

testLocate :: IO ()
testLocate = do
    m <- testMacro
    let (Just (obj, next)) = A.locate subleqMA 100 (fromJust $ A.lookupModule "mult" m)
    putStrLn $ render $ A.printObject obj
    print next

testLocateModulePacked :: IO ()
testLocateModulePacked = do
    mo <- testMacro
    let (end, ma) = A.locateModulePacked subleqMA 100 mo
    putStrLn $ render $ vcat $ map (\(addr, obj) -> text "Address" <+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
    print end

testLoadModulePacked :: IO ()
testLoadModulePacked = do
    mo <- testMacro
    print $ A.loadModulePacked subleqMA 100 mo M.empty
    -- let (end, ma) = A.loadModulePacked subleqMA 100 mo
    -- putStrLn $ render $ vcat $ map (\(addr, obj) -> text "Address" <+> integer addr <> colon $$ A.printObject obj ) $ M.elems ma
    -- print end

testAdd :: IO ()
testAdd = do
    mo <- testMacro
    let (a, b) = (3, 8)
    let (_, pos, mem) = A.loadModulePacked subleqMA 100 mo M.empty
    let Just addrAdd = M.lookup "add" pos
    let mem' = Mem.write 2 a . Mem.write 3 b $ mem
    let hist = runMachineHist Subleq.step (addrAdd, mem')
    putStrLn $ printf "%d + %d = %d" a b (Mem.read 1 . snd . last $ hist)
    putStrLn "Traces:"
    print hist

main :: IO ()
main = undefined
