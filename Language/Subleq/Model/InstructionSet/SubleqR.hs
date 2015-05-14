{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Language.Subleq.Model.InstructionSet.SubleqR (step, initialMachine, BitReversable(..)) where

import Data.Bits
import Language.Subleq.Model.Prim
import qualified Language.Subleq.Model.Memory as Mem

explosion b 0 = [0]
explosion b x | b + x == b - 1 = [b + x]
              | x < 0 && (-x) < b = [b + x, b - 1]
explosion b x = r : explosion b q
    where
      (q,r) = x `divMod` b

implosion b [s] | s == 0 = 0
                | s == b - 1 = -1
implosion b (x:xs) = x + b * implosion b xs

extend 0 _ = []
extend _ [] = []
extend 1 (x:xs) = [x]
extend n [x] = x : extend (n - 1) [x]
extend n (x:xs) = x : extend (n - 1) xs

bitRev n = implosion 2 . reverse . extend n . explosion 2

bitRevFix :: (FiniteBits a) => a -> a
bitRevFix n = foldl setBit zeroBits bs'
    where
      l = finiteBitSize n
      bs = filter (testBit n) [0..(l-1)]
      bs' = map (\x -> l - x - 1) bs

bitLen = length . explosion 2

bitRevSub a b = r (r a - r b)
        where
          l = length (explosion 2 a) `max` length (explosion 2 b)
          r = bitRev l
          r' = bitRev (l + 1)

class BitReversable a where
    reversal :: Int -> a -> a
    reversalSub :: a -> a -> a

instance BitReversable Integer where
    reversal = bitRev
    reversalSub a b = r (r a - r b)
        where
          l = length (explosion 2 a) `max` length (explosion 2 b)
          r = bitRev l

instance (Num a, FiniteBits a, Bounded a) => BitReversable a where
    reversal _ = bitRevFix
    reversalSub a b = r (r a - r b)
        where
          r = bitRevFix

step :: (Memory a a m, Num a, Ord a, Integral a, BitReversable a) => Machine a a m Bool
step = do
    pc <- getPC
    pA <- readMem pc
    pB <- readMem $ pc + 1
    pC <- readMem $ pc + 2
    a <- readMem pA
    b <- readMem pB
    let b' = if pC < 0 then reversalSub b a else b - a
    let cond = if pC < 0 then b' == 0 || b' `mod` 2 == 1 else b' <= 0
    let pc' = if cond then abs pC else pc + 3
    writeMem pB b'
    putPC pc'
    return $ pC == 0

initialMachine :: (Memory a a m, Num a, Ord a, Enum a) => SubleqState a w m
initialMachine = (6, Mem.fromAssocList . zip [0..] $ [ 0, 3, 5, 1, 0, 0
                                                     , 1, 0, 9
                                                     , 2, 0, 12
                                                     , 3, 3, 15
                                                     , 0, 3, 18
                                                     , 0, 0, -1])
