{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, FunctionalDependencies #-}
module Language.Subleq.Model.Memory (Address(advance), Memory(read, write, empty, fromAssocList)) where

import Prelude hiding (read)
import Data.Map (Map)
import qualified Data.Map as M

class Address a where
    advance :: (Integral b) => b -> a -> a

instance (Num a) => Address a where
    advance d = (+ fromIntegral d)

class Memory a w m | m -> w, m -> a where
    read :: a -> m -> w
    write  :: a -> w -> m -> m
    empty ::  m
    fromAssocList :: [(a, w)] -> m
    fromAssocList = foldr (uncurry write) empty

instance (Address a, Num b, Ord a) => Memory a b (Map a b) where
    read = M.findWithDefault 0
    write = M.insert
    empty = M.empty

