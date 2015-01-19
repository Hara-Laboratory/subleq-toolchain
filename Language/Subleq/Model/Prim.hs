{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Language.Subleq.Model.Prim (Address, Memory, SubleqState, Machine, getPC, putPC, readMem, writeMem, advancePC, runMachineStep, runMachine, runMachineWithHistory) where

import Language.Subleq.Model.Memory (Address, Memory)
import qualified Language.Subleq.Model.Memory as Mem
import Control.Monad.State
import Control.Arrow

type SubleqState a w m = (a, m)

type Machine a w m = State (SubleqState a w m)

getPC :: Machine a w m a
getPC = get >>= \(pc,_)-> return pc

putPC :: a -> Machine a w m ()
putPC pc = get >>= \(_,mem)-> put (pc,mem)

readMem :: (Memory a w m) => a -> Machine a w m w
readMem addr = get >>= \(_,mem)-> return (Mem.read addr mem)

writeMem :: (Memory a w m) => a -> w -> Machine a w m ()
writeMem addr val = get >>= \(pc,mem)-> put (pc, Mem.write addr val mem)

advancePC :: (Integral a, Memory a w m) => a -> Machine a w m ()
advancePC d = do
    pc <- getPC
    putPC (pc + fromIntegral d)

runMachineStep :: Machine a w m Bool -> SubleqState a w m -> SubleqState a w m
runMachineStep st m = m'
    where
      (_, m') = runState st m

runMachineHist :: Machine a w m Bool -> SubleqState a w m -> [SubleqState a w m]
runMachineHist st m = cont `seq` if cont then m':runMachineHist st m' else [m']
    where
      (cont, m') = runState st m

runMachineWithHistory :: Machine a w m Bool -> SubleqState a w m -> (SubleqState a w m, [SubleqState a w m])
runMachineWithHistory st m = cont `seq` if cont then second (m':) $ runMachineWithHistory st m' else (m', [m'])
    where
      (cont, m') = runState st m

runMachine :: Machine a w m Bool -> SubleqState a w m -> SubleqState a w m
runMachine st m = cont `seq` if cont then runMachine st m' else m'
    where
      (cont, m') = runState st m
