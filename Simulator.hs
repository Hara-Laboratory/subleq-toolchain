{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies #-}
module Simulator (Word, Address, Memory, SubleqState, IntMachine, IntSubleqState, Machine, getPC, putPC, readMem, writeMem, advancePC, subleqSteop, initialMachine, runMachineStep, runMachine) where

import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State
import Control.Lens

type Word = Integer

class Address a where
    advance :: (Integral b) => b -> a -> a

instance (Num a) => Address a where
    advance d = (+ fromIntegral d)

class Memory a w m | m -> w, m -> a where
    readMemInternal :: a -> m -> w
    writeMemInternal  :: a -> w -> m -> m

instance (Address a, Num b, Ord a) => Memory a b (Map a b) where
    readMemInternal addr mem = M.findWithDefault 0 addr mem
    writeMemInternal addr val mem = M.insert addr val mem

type SubleqState a w m = (a, m)
type IntMachine = Machine Integer Integer (Map Integer Word)
type IntSubleqState = (Integer, Map Integer Word)

type Machine a w m = State (SubleqState a w m)

getPC :: Machine a w m a
getPC = get >>= \(pc,mem)-> return pc

putPC :: a -> Machine a w m ()
putPC pc = get >>= \(_,mem)-> put (pc,mem)

readMem :: (Memory a w m) => a -> Machine a w m w
readMem addr = get >>= \(_,mem)-> return (readMemInternal addr mem)

writeMem :: (Memory a w m) => a -> w -> Machine a w m ()
writeMem addr val = get >>= \(pc,mem)-> put (pc, writeMemInternal addr val mem)

advancePC :: (Integral a, Memory a w m) => a -> Machine a w m ()
advancePC d = do
    pc <- getPC
    putPC (pc + fromIntegral d)

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

runMachineStep :: Machine a w m Bool -> SubleqState a w m -> SubleqState a w m
runMachineStep st m = m'
    where
      (cont, m') = runState st m

runMachine :: Machine a w m Bool -> SubleqState a w m -> [SubleqState a w m]
runMachine st m = cont `seq` if cont then m':runMachine st m' else [m']
    where
      (cont, m') = runState st m
