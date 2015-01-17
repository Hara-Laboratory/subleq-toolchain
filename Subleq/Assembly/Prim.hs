module Subleq.Assembly.Prim where

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

type Module = [Object]
--    deriving (Read, Show, Eq, Ord)

