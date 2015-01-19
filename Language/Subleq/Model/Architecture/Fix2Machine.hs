{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}
module Language.Subleq.Model.Architecture.Fix2Machine (Word
                                                      , Fix2Machine16, Fix2SubleqState16
                                                      , Fix2Machine32, Fix2SubleqState32
                                                      , Fix2Machine64, Fix2SubleqState64
                                                      ) where

import Language.Subleq.Model.Prim
import Data.Map (Map)
import Data.Word

type Fix2Machine16 = Machine Word16 Word16 (Map Word16 Word)
type Fix2SubleqState16 = (Word16, Map Word16 Word)

type Fix2Machine32 = Machine Word32 Word32 (Map Word32 Word)
type Fix2SubleqState32 = (Word32, Map Word32 Word)

type Fix2Machine64 = Machine Word64 Word64 (Map Word64 Word)
type Fix2SubleqState64 = (Word64, Map Word64 Word)
