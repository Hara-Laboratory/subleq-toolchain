module Subleq.Assembly.Prim where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

type Id = String
type Location = String

data Expr = Identifier Id
          | Number Integer
    deriving (Read, Show, Eq, Ord)

type LocExpr = (Maybe Location, Expr)
--    deriving (Read, Show, Eq, Ord)

data Instruction = Subleq
    deriving (Read, Show, Eq, Ord)

data Element = ElemInst Instruction [LocExpr]
             | SubroutineCall (Maybe Location) Id [Expr]
    deriving (Read, Show, Eq, Ord)

data Object = Subroutine Id [Id] [Element]
            | Macro Id [Id] [Element]
    deriving (Read, Show, Eq, Ord)

data Module = Module (Map Id Object)
    deriving (Read, Show, Eq, Ord)

