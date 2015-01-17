module Subleq.Assembly.Prim where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

type Id = String
type Location = String
type Substitution = Map Id Expr

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

maybeToSet :: Maybe a -> Set a
maybeToSet = maybe S.empty S.singleton

elemsSet :: (Ord a)=>Map k a -> Set a
elemsSet m = S.fromList (M.elems m)

unionsMap :: (Ord b)=>(a -> Set b) -> Map k a -> Set b
unionsMap f m = fst $ M.mapAccum (\a b-> (S.union a (f b), ())) S.empty m

objectId :: Object -> Id
objectId (Subroutine n _ _) = n
objectId (Macro n _ _) = n

substituteExpr :: Substitution -> Expr -> Expr
substituteExpr sub i@(Identifier x)  = M.findWithDefault i x sub
substituteExpr _   e'                = e'

substituteLocExpr :: Substitution -> LocExpr -> LocExpr
substituteLocExpr sub (l, e') = (l, substituteExpr sub e')

substituteElement :: Substitution -> Element -> Element
substituteElement sub (ElemInst i es) = ElemInst i (map (substituteLocExpr sub) es)
substituteElement sub (SubroutineCall l i es) = SubroutineCall l i (map (substituteExpr sub) es)

substituteObject :: Substitution -> Object -> Object
substituteObject sub (Subroutine n args elems) = Subroutine n args $ map (substituteElement sub) elems
substituteObject sub (Macro n args elems)      = Macro n args $ map (substituteElement sub) elems

locationsElement :: Element -> Set Id
locationsElement (ElemInst _ es) = S.fromList $ mapMaybe fst es
locationsElement (SubroutineCall l _ _) = maybeToSet l

locationsObject :: Object -> Set Id
locationsObject (Subroutine _ _ elems) = S.unions $ map locationsElement elems
locationsObject (Macro _ _ elems) = S.unions $ map locationsElement elems

boundedVars :: Object -> Set Id
boundedVars o@(Subroutine _ args _) = S.fromList args `S.union` locationsObject o
boundedVars o@(Macro _ args _) = S.fromList args `S.union` locationsObject o

freeVarExpr :: Expr -> Set Id
freeVarExpr (Identifier i) = S.singleton i
freeVarExpr _              = S.empty

freeVarLocExpr :: LocExpr -> Set Id
freeVarLocExpr (_,e) = freeVarExpr e

freeVarElement :: Element -> Set Id
freeVarElement (ElemInst _ es) = S.unions $ map freeVarLocExpr es
freeVarElement (SubroutineCall l x es) = S.unions $ [maybeToSet l, S.singleton x] ++ map freeVarExpr es

freeVarObject :: Object -> Set Id
freeVarObject o@(Subroutine _ args es) =  S.unions (map freeVarElement es) S.\\ S.fromList args S.\\ locationsObject o
freeVarObject o@(Macro _ args es) =  S.unions (map freeVarElement es) S.\\ S.fromList args S.\\ locationsObject o

freeVarModule :: Module -> Set Id
freeVarModule (Module m) =  unionsMap freeVarObject m S.\\ M.keysSet m
