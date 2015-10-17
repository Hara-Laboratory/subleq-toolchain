{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DeriveDataTypeable, TemplateHaskell #-}
module Language.Subleq.Configure where

-- import Language.Subleq.Model.Prim
import qualified Language.Subleq.Model.Memory as Mem
-- import Language.Subleq.Model.Architecture.IntMachine
import qualified Language.Subleq.Model.InstructionSet.Subleq as Subleq
import qualified Language.Subleq.Assembly as A
import Language.Subleq.Assembly.Export.Elf2Mem
import Text.Parsec
import Control.Applicative
import Text.PrettyPrint
import qualified Text.PrettyPrint as PP
import Text.Printf
import Data.Maybe
import Data.List
-- import Data.Map (Map)
import qualified Data.Map as M
-- import Control.Monad.State
import Control.Lens
import Data.Typeable
import Data.Data

data LocationMethod = SequenceFrom Int
                    | UseFrom [Int]
    deriving (Read, Show, Data, Typeable)

data SubleqConfig = SubleqConfig { _instructionLength :: Integer
                                 , _wordLengthInAddressingUnit :: Integer
                                 , _wordLengthInBit :: Integer
                                 , _argumentLocations :: LocationMethod
                                 , _staticLocations :: [(A.Id, Integer)]
                                 }
            deriving (Read, Show, Data, Typeable)
makeLenses ''SubleqConfig

defaultConfig = SubleqConfig { _instructionLength = 3
                             , _wordLengthInAddressingUnit = 1
                             , _wordLengthInBit = 32
                             , _argumentLocations = SequenceFrom 0
                             , _staticLocations = [ ("Lo", 32)
                                                  , ("Hi", 33)
                                                  , ("End", -1)
                                                  , ("Z",  23)
                                                  , ("T0", 16)
                                                  , ("T1", 17)
                                                  , ("T2", 18)
                                                  , ("T3", 19)
                                                  , ("T4", 20)
                                                  , ("T5", 21)
                                                  , ("T6", 22)
                                                  , ("CW", 26)
                                                  , ("Inc", 24)
                                                  , ("Dec", 25)
                                                  ]
                             }

cases2015Config = SubleqConfig { _instructionLength = 3
                             , _wordLengthInAddressingUnit = 1
                             , _wordLengthInBit = 32
                             , _argumentLocations = UseFrom [37, 38, 39]
                             , _staticLocations = [ ("Lo", 32)
                                                  , ("Hi", 33)
                                                  , ("End", 999)
                                                  , ("Z",  36)
                                                  , ("T0", 40)
                                                  , ("T1", 41)
                                                  , ("T2", 42)
                                                  , ("T3", 43)
                                                  , ("T4", 44)
                                                  , ("T5", 45)
                                                  , ("T6", 46)
                                                  , ("CW", 47)
                                                  , ("Inc", 48)
                                                  , ("Dec", 49)
                                                  ]
                             }

memoryArchitectureFromConfig :: SubleqConfig -> A.MemoryArchitecture (M.Map Integer Integer)
memoryArchitectureFromConfig c = A.MemoryArchitecture { A.instructionLength = c^.instructionLength
                                                      , A.wordLength = c^.wordLengthInAddressingUnit
                                                      , A.locateArg = locateArg' . map fromIntegral . useAddrs $ c^.argumentLocations
                                                      , A.locateStatic = M.fromList $ c^.staticLocations
                                                      , A.writeWord = Mem.write
                                                      }
    where
      useAddrs (SequenceFrom n) = [n..]
      useAddrs (UseFrom ns) = ns
      locateArg' as xs = M.fromList $ zip xs as

initialValues :: (Num a, Integral a) => a -> M.Map String a
initialValues wl = M.fromList [ ("Inc", -1)
                              , ("Dec",  1)
                              , ("CW", wl)
                              , ("Min", - (2^(wl - 1)))
                              , ("Max", 2^(wl - 1) - 1)
                              , ("LMax", 2^(wl `div` 2) - 1)
                              , ("LMin", (2^(wl `div` 2 - 1)))
                              , ("HDec", (2^(wl `div` 2)))
                              , ("HInc", - (2^(wl `div` 2)))
                              ]

initialMemFromConfigure :: (Num a, Ord a) => SubleqConfig -> M.Map a a
initialMemFromConfigure c = foldr (uncurry Mem.write) M.empty ls
    where
      ls = mapMaybe f sls
      sls = mapped._2 %~ fromIntegral $ (c^.staticLocations)   
      f :: (Num a)=>(A.Id, Integer) -> Maybe (a, a)
      f (i, l) = do v <- i `M.lookup` initialValues (c^.wordLengthInBit)
                    return (fromIntegral l, fromIntegral v)
