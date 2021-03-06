module Language.Subleq.Assembly.Prim where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf
import Data.List
import Data.Bits

type Id = String
type Location = String
type Substitution = Map Id Expr

data Expr = Identifier Id
          | Number Integer
          | EAdd Expr Expr
          | ESub Expr Expr
          | EShiftL Expr Expr
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

objectArity :: Object -> Int
objectArity (Subroutine _ args _) = length args
objectArity (Macro _ args _) = length args

evaluateNumExpr :: Expr -> Integer
evaluateNumExpr (Identifier x) = error $ "unexpected identifier " ++ x ++ "."
evaluateNumExpr (Number n) = n
evaluateNumExpr (EAdd e1 e2) = evaluateNumExpr e1 + evaluateNumExpr e2
evaluateNumExpr (ESub e1 e2) = evaluateNumExpr e1 - evaluateNumExpr e2
evaluateNumExpr (EShiftL e1 e2) = evaluateNumExpr e1 `shift` fromIntegral (evaluateNumExpr e2)

evaluateNumExprInLocElem :: LocExpr -> LocExpr
evaluateNumExprInLocElem (l, e) = (l, Number $ evaluateNumExpr e)

evaluateNumExprInElem :: Element -> Element
evaluateNumExprInElem (ElemInst i les) = ElemInst i $ map evaluateNumExprInLocElem les
evaluateNumExprInElem e@(SubroutineCall {}) = e
evaluateNumExprInElem e@(ElemLoc {}) = e

substituteExpr :: Substitution -> Expr -> Expr
substituteExpr sub i@(Identifier x)  = M.findWithDefault i x sub
substituteExpr sub i@(EAdd e1 e2)  = EAdd (substituteExpr sub e1) (substituteExpr sub e2)
substituteExpr sub i@(ESub e1 e2)  = ESub (substituteExpr sub e1) (substituteExpr sub e2)
substituteExpr sub i@(EShiftL e1 e2)  = EShiftL (substituteExpr sub e1) (substituteExpr sub e2)
substituteExpr _ (Number n) = Number n

substituteLocId :: Substitution -> Id -> Id
substituteLocId sub l | l `M.member` sub = case M.lookup l sub of
                                             Just (Identifier l') -> l'
                                             -- Just x -> error $ printf "Label %s cannot be substituted with %s" l (show x)
                                             Just x -> printf "%s(%s)" l (show x)
                                             -- Just _ -> (Nothing, substituteExpr sub e')
                                             Nothing -> l
substituteLocId _   l = l

substituteLocExpr :: Substitution -> LocExpr -> LocExpr
substituteLocExpr sub (Just l, e') = (Just (substituteLocId sub l), substituteExpr sub e')
substituteLocExpr sub (l, e') = (l, substituteExpr sub e')

substituteElement :: Substitution -> Element -> Element
substituteElement sub (ElemInst i es) = ElemInst i (map (substituteLocExpr sub) es)
substituteElement sub (SubroutineCall l i es) = SubroutineCall (fmap (substituteLocId sub) l) i (map (substituteExpr sub) es)
substituteElement sub (ElemLoc l) = ElemLoc $ substituteLocId sub l

substituteObject :: Substitution -> Object -> Object
substituteObject sub (Subroutine n args elems) = Subroutine n args $ map (substituteElement sub) elems
substituteObject sub (Macro n args elems)      = Macro n args $ map (substituteElement sub) elems

locationsElement :: Element -> Set Id
locationsElement (ElemInst _ es) = S.fromList $ mapMaybe fst es
locationsElement (ElemLoc l) = S.singleton l
locationsElement (SubroutineCall l _ _) = maybeToSet l

locationsElements :: [Element] -> Set Id
locationsElements = S.unions . map locationsElement

locationsObject :: Object -> Set Id
locationsObject (Subroutine _ _ elems) = S.unions $ map locationsElement elems
locationsObject (Macro _ _ elems) = S.unions $ map locationsElement elems

freqMap :: (Ord a)=>[a] -> M.Map a Int
freqMap xs = M.fromListWith (+) . zip xs $ repeat 1

locationsOccursionElement :: Element -> Map Id Int
locationsOccursionElement (ElemInst _ es) = freqMap $ mapMaybe fst es
locationsOccursionElement (ElemLoc l) = M.singleton l 1
locationsOccursionElement (SubroutineCall Nothing _ _) = M.empty
locationsOccursionElement (SubroutineCall (Just l) _ _) = M.singleton l 1

locationsOccursionElements :: [Element] -> Map Id Int
locationsOccursionElements = M.unionsWith (+) . map locationsOccursionElement

locationsOccursionObject :: Object -> Map Id Int
locationsOccursionObject (Subroutine _ _ elems) = locationsOccursionElements elems
locationsOccursionObject (Macro _ _ elems) = locationsOccursionElements elems

errorsObject :: Object -> [String]
errorsObject (Subroutine n args elems) = errorsObject' n args elems
errorsObject (Macro n args elems) = errorsObject' n args elems

errorsObject' :: Id -> [Id] -> [Element] -> [String]
errorsObject' n args elems = catMaybes [e1, e2, e3]
    where
      e1 | not . null $ dupLocs =
           Just $
             printf "Object %s: locations must be exclusive, but: %s" n (show dupLocs)
         | otherwise = Nothing
      e2 | not . null $ dupArgs =
           Just $
             printf "Object %s: arguments must be exclusive, but: %s" n (show dupArgs)
         | otherwise = Nothing
      e3 | not . null $ dupArgLocs =
           Just $
             printf "Object %s: locations and arguments must be exclusive, but: %s" n (show dupArgLocs)
         | otherwise = Nothing
      argFreq = freqMap args
      locFreq = locationsOccursionElements elems
      dupArgs = M.elems . M.filter (> 1) $ argFreq
      dupLocs = M.elems . M.filter (> 1) $ locFreq
      dupArgLocs = M.elems . M.filter (> 1) $ M.unionWith (+) argFreq locFreq

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

applyObject :: LabelPrefix -> Object -> [Expr] -> [Element]
applyObject lp (Macro x as es) =  applyObject' lp x as es
applyObject _  (Subroutine x _ _) = error $ printf "%s is a subroutine and not applicable" x

applyObject' :: LabelPrefix -> Id -> [Id] -> [Element] -> [Expr] -> [Element]
applyObject' lp x as es aes | length as == length aes = map (substituteElement sub) $ addLocationPrefix lp targets es -- addLocationPrefix lp $ map (substituteElement sub) es
                            | otherwise               = error $ printf "%s takes %d argument(s), but got: %s" x (length as) (show aes)
    where
      sub = M.fromList $ zip (map (labelPrefixToString lp ++) as) aes
      targets = S.fromList as `S.union` locationsElements es

type DistinctStack a = ([a], Set a)

push :: (Ord a)=>a -> DistinctStack a -> Maybe (DistinctStack a)
push x (xs, st) | x `S.member` st = Nothing
                | otherwise       = Just (x:xs, S.insert x st)

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
expandMacro m (Subroutine x as es) = Subroutine x as (concatMap (\(i, e)->expandMacro' (singletonStack x) m [i] e) $ zip [0..] es)

expandMacro' :: DistinctStack Id -> Module -> LabelPrefix -> Element -> [Element]
expandMacro' stk m lp (SubroutineCall l x as) = es''
    where
      stk' = fromMaybe
               (error $ printf "%s: Cyclic macro expansion: %s" x (show $ stackToList stk))
               (push x stk)
      o :: Object
      o = fromMaybe
            (error $ printf "Object %s is not found in the module: %s" x (show $ stackToList stk))
            (lookupModule x m)
      es' :: [Element]
      es' = map ElemLoc (maybeToList l) ++ applyObject lp o as
      es'' = concatMap (\(i, e)-> expandMacro' stk' m (i:lp) e) $ zip [0..] es'
expandMacro' _ _ _ e@(ElemInst _ _) = [e]
expandMacro' _ _ _ e@(ElemLoc _) = [e]

addLocationPrefix :: LabelPrefix -> Set Id -> [Element] -> [Element]
addLocationPrefix lp targets elems = elems'
    where
      elems' = map (substituteElement sub) elems
      sub = M.fromSet (Identifier . (labelPrefixToString lp ++)) targets

type LabelPrefix = [Int]

labelPrefixToString :: LabelPrefix -> String
labelPrefixToString = ('_':) . intercalate "_" . reverse . map show
