{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Language.Subleq.Model.Architecture.IntMachine (Word, IntMachine, IntSubleqState) where

import Language.Subleq.Model.Prim
import Data.Map (Map)

type Word = Integer

type IntMachine = Machine Integer Integer (Map Integer Word)
type IntSubleqState = (Integer, Map Integer Word)
