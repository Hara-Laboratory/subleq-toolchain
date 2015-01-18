module Subleq.Assembly.Locate where

import Subleq.Assembly.Prim
import qualified Subleq.Assembly.Prim as A
-- import Subleq.Model
-- import Data.Maybe
-- import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Text.Printf
import Control.Monad.State
-- import Data.List

data MemoryArchitecture = MemoryArchitecture { wordLength :: Integer
                                             , instructionLength :: Integer
                                             , locateArg :: LocateArg
                                             , locateStatic :: Map Id Integer
                                             }

type LocateArg = [Id] -> Map Id Integer

locateArgDefault :: LocateArg
locateArgDefault xs = M.fromList $ zip xs [1..]

locateLocExpr :: MemoryArchitecture -> Integer -> [LocExpr] -> Map A.Id Integer
locateLocExpr _  _ [] = M.empty
locateLocExpr ma i ((Nothing, _):es) = locateLocExpr ma (i + wordLength ma) es
locateLocExpr ma i ((Just l, _):es) = M.insert l i $ locateLocExpr ma (i + wordLength ma) es

locate' :: MemoryArchitecture -> [Element] -> State Integer (Map A.Id Integer)
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

locate :: MemoryArchitecture -> Integer -> Object -> Maybe (Object, Integer)
locate ma i o@(Subroutine _ args es) = Just (substituteObject sub o, next)
    where
      (mp, next) = runState (locate' ma es) i
      sub = M.map A.Number $ M.unions [locateStatic ma, locateArg ma args, mp]

locate _  _ (Macro {}) = Nothing


