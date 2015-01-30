{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Language.Subleq.Model.InstructionSet.Subneg (step, initialMachine) where

import Language.Subleq.Model.Prim
import qualified Language.Subleq.Model.Memory as Mem

step :: (Memory a a m, Num a, Ord a) => Machine a w m Bool
step = do
    pc <- getPC
    pA <- readMem pc
    pB <- readMem $ pc + 1
    pC <- readMem $ pc + 2
    a <- readMem pA
    b <- readMem pB
    let b' = b - a
    let pc' = if b' < 0 then pC else pc + 3
    writeMem pB b'
    putPC pc'
    return $ pC >= 0

initialMachine :: (Memory a a m, Num a, Ord a, Enum a) => SubleqState a w m
initialMachine = (6, Mem.fromAssocList . zip [0..] $ [ 0, 3, 5, 1, 0, 0
                                                     , 1, 0, 9
                                                     , 2, 0, 12
                                                     , 3, 3, 15
                                                     , 0, 3, 18
                                                     , 0, 0, -1])
