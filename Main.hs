{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Main where

import Subleq.Model.Prim
import Subleq.Model.Architecture.IntMachine
import qualified Subleq.Model.InstructionSet.Subleq as Subleq
import qualified Subleq.Assembly.Parser as A
import Text.Parsec
import Control.Applicative
-- import Data.Map (Map)
-- import qualified Data.Map as M
-- import Control.Monad.State
-- import Control.Lens

testSubleq :: [IntSubleqState]
testSubleq = runMachineHist Subleq.step Subleq.initialMachine

testParser :: IO (Either ParseError A.Module)
testParser = parse A.parseModule "parserModule" <$> readFile "test.sq"

main :: IO ()
main = undefined
