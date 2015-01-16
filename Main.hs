{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies #-}
module Main where

import Simulator
import Assembler
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Lens

step :: IntMachine Bool
step = do
    pc <- getPC
    pA <- readMem $ pc
    pB <- readMem $ pc + 1
    pC <- readMem $ pc + 2
    a <- readMem pA
    b <- readMem pB
    let b' = b - a
    let pc' = if b' < 0 then pC else pc + 3
    writeMem pB b'
    putPC pc'
    return $ pC >= 0

initialMachine :: IntSubleqState
initialMachine = (6, M.fromList . zip [0..] $ [ 0, 3, 5, 1, 0, 0
                                              , 1, 0, 9
                                              , 2, 0, 12
                                              , 3, 3, 15
                                              , 0, 3, 18
                                              , 0, 0, -1])

main :: IO ()
main = undefined
