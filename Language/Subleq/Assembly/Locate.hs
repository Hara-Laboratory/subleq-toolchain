{-# LANGUAGE Rank2Types #-}
module Language.Subleq.Assembly.Locate where

import Language.Subleq.Assembly.Prim
import qualified Language.Subleq.Assembly.Prim as A
-- import Subleq.Model
-- import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf
import Control.Monad.State
-- import Data.List

data MemoryArchitecture m = MemoryArchitecture { wordLength :: Integer
                                               , instructionLength :: Integer
                                               , locateArg :: LocateArg
                                               , locateStatic :: Map Id Integer
                                               , writeWord :: Integer -> Integer -> m -> m
                                               }

type LocateArg = [Id] -> Map Id Integer

locateArgDefault :: LocateArg
locateArgDefault xs = M.fromList $ zip xs [1..]

locateLocExpr :: MemoryArchitecture m -> Integer -> [LocExpr] -> Map A.Id Integer
locateLocExpr _  _ [] = M.empty
locateLocExpr ma i ((Nothing, _):es) = locateLocExpr ma (i + wordLength ma) es
locateLocExpr ma i ((Just l, _):es) = M.insert l i $ locateLocExpr ma (i + wordLength ma) es

locate' :: MemoryArchitecture m -> [Element] -> State Integer (Map A.Id Integer)
locate' _ [] = return M.empty
locate' ma (ElemInst Subleq es : elems) = do
    i <- get
    let loc = locateLocExpr ma i es
    modify (+ instructionLength ma)
    loc' <- locate' ma elems
    return $ loc `M.union` loc'
locate' ma (ElemLoc l : elems) = do
    i <- get
    loc <- locate' ma elems
    return $ M.insert l i loc
locate' _  (SubroutineCall {} : _) = error $ printf "locate: please do macro expansion first."

locate :: MemoryArchitecture m -> Integer -> Object -> Maybe (Object, Integer)
locate ma i o@(Subroutine _ args es) = Just (substituteObject sub o, next)
    where
      (mp, next) = runState (locate' ma es) i
      sub = M.map A.Number $ M.unions [locateStatic ma, locateArg ma args, mp]

locate _  _ (Macro {}) = Nothing

locateModulePacked :: MemoryArchitecture m -> Integer -> Module -> (Integer, Map Id (Integer, Object))
locateModulePacked ma initialAddr (Module mo) = M.foldrWithKey f (initialAddr, M.empty) mo
    where
      f :: (Id -> Object -> (Integer, Map Id (Integer, Object)) -> (Integer, Map Id (Integer, Object)))
      f x obj (i, mp) = case locate ma i obj of
                          Nothing -> (i, mp)
                          Just (obj', i') -> (i', M.insert x (i, obj') mp)

loadElement :: MemoryArchitecture m -> Integer -> Element -> m -> (m, Integer)
loadElement ma i (ElemInst Subleq [(_, Number x)])                                  m = loadElement ma i (ElemInst Subleq $ map (\z->(Nothing, z)) [Number x, Number x]) m
loadElement ma i (ElemInst Subleq [(_, Number x), (_, Number y)])                   m = loadElement ma i (ElemInst Subleq $ map (\z->(Nothing, z)) [Number x, Number y, Number (i + instructionLength ma)]) m
loadElement ma i (ElemInst Subleq [(_, Number x), (_, Number y), (_, Number z)])    m = (writeWord ma i x $ writeWord ma (i + wl) y $ writeWord ma (i + 2 * wl) z m, i + 3 * wl)
    where
      wl = wordLength ma
loadElement _  i (ElemLoc _) m = (m, i)
loadElement _  i e@(SubroutineCall {})    _ = error $ printf "loadElement: addr %d: macro expansion (%s) is not expandable" i (show e)
loadElement _  i e@(ElemInst {})    _ = error $ printf "loadElement: addr %d: instruction (%s) is not expandable" i (show e)

loadElements :: MemoryArchitecture m -> Integer -> [Element] -> m -> m
loadElements ma i elems m = fst $ Prelude.foldl (\(mem, next) el->loadElement ma next (evaluateNumExprInElem el) mem) (m, i) elems

loadObject :: MemoryArchitecture m -> Integer -> Object -> m -> m
loadObject ma i (Subroutine _ _ elems) = loadElements ma i elems
loadObject ma i (Macro _ _ elems)      = loadElements ma i elems

loadModulePacked :: MemoryArchitecture m -> Integer -> Module -> m -> (Integer, Map Id Integer, m)
loadModulePacked ma i mo mem = (end, allocation, M.foldr (uncurry $ loadObject ma) mem mao)
    where
      (end, mao) = locateModulePacked ma i mo
      allocation = M.map fst mao

