module Subleq.Assembly.Prim where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf

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

instructionArity :: Instruction -> (Int, Int)
instructionArity Subleq = (1, 3)

data Element = ElemInst Instruction [LocExpr]
             | SubroutineCall (Maybe Location) Id [Expr]
             | ElemLoc Location
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
substituteElement _   e@(ElemLoc _) = e

substituteObject :: Substitution -> Object -> Object
substituteObject sub (Subroutine n args elems) = Subroutine n args $ map (substituteElement sub) elems
substituteObject sub (Macro n args elems)      = Macro n args $ map (substituteElement sub) elems

locationsElement :: Element -> Set Id
locationsElement (ElemInst _ es) = S.fromList $ mapMaybe fst es
locationsElement (ElemLoc l) = S.singleton l
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
freeVarElement (ElemLoc _) = S.empty

freeVarObject :: Object -> Set Id
freeVarObject o@(Subroutine _ args es) =  S.unions (map freeVarElement es) S.\\ S.fromList args S.\\ locationsObject o
freeVarObject o@(Macro _ args es) =  S.unions (map freeVarElement es) S.\\ S.fromList args S.\\ locationsObject o

freeVarModule :: Module -> Set Id
freeVarModule (Module m) =  unionsMap freeVarObject m S.\\ M.keysSet m

applyObject :: Object -> [Expr] -> [Element]
applyObject (Macro x as es) =  applyObject' x as es
applyObject (Subroutine x _ _) = error $ printf "%s is a subroutine and not applicable" x

applyObject' :: Id -> [Id] -> [Element] -> [Expr] -> [Element]
applyObject' x as es aes | length as == length aes = map (substituteElement sub) es
                         | otherwise               = error $ printf "%s takes %d argument(s), but got: %s" x (show $ length as) (show aes)
    where
      sub = M.fromList $ zip as aes

type DistinctStack a = ([a], Set a)

push :: (Ord a)=>a -> DistinctStack a -> Maybe (DistinctStack a)
push x (xs, st) | x `S.member` st = Just (x:xs, S.insert x st)
                | otherwise       = Nothing

pop :: (Ord a)=>DistinctStack a -> Maybe (a, DistinctStack a)
pop ([], _) = Nothing
pop (x:xs, st) = Just (x, (xs, S.delete x st))

emptyStack :: DistinctStack a
emptyStack = ([], S.empty)

singletonStack :: (Ord a)=>a -> DistinctStack a
singletonStack x = ([x], S.singleton x)

stackToList :: DistinctStack a -> [a]
stackToList = fst

lookupModule :: Id -> Module -> Maybe Object
lookupModule x (Module m) = M.lookup x m

expandMacroAll :: Module -> Module
expandMacroAll m@(Module m') = Module $ M.map (expandMacro m) m'

expandMacro :: Module -> Object -> Object
expandMacro _ o@(Macro {}) =  o
expandMacro m (Subroutine x as es) = Subroutine x as (concatMap (expandMacro' (singletonStack x) m) es)

expandMacro' :: DistinctStack Id -> Module -> Element -> [Element]
expandMacro' stk m (SubroutineCall l x as) = es''
    where
      stk' = fromMaybe
               (error $ printf "Cyclic macro expansion: %s" (show $ stackToList stk))
               (push x stk)
      o :: Object
      o = fromMaybe
            (error $ printf "Object %s is not found in the module: %s" x (show $ stackToList stk))
            (lookupModule x m)
      es' :: [Element]
      es' = map ElemLoc (maybeToList l) ++ applyObject o as
      es'' = concatMap (expandMacro' stk' m) es'
expandMacro' _ _ e@(ElemInst _ _) = [e]
expandMacro' _ _ e@(ElemLoc _) = [e]
