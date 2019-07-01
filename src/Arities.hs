module Arities (arities) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Types

arities :: Map MvInstr Arity
arities = M.fromList
  [ (MNoop, Arity 0)
  , (MMult, Arity 2)
  , (MAdd, Arity 2)
  , (MSub, Arity 2)
  , (MDiv, Arity 2)
  , (MMod, Arity 2)
  , (MFloor, Arity 1)
  , (MCeil, Arity 1)
  , (MRound, Arity 1)
  , (MNegate, Arity 1)
  , (MLT, Arity 2)
  , (MGT, Arity 2)
  , (MEQ, Arity 2)
  , (MNot, Arity 1)
  , (MDup, Arity 1)
  , (MSwap, Arity 2)
  , (MDrop, Arity 1)
  , (MRotU, Stack)
  , (MRotD, Stack)
  , (MReverse, Stack)
  , (MLength, Stack)
  , (MRecurse, Arity 0)
  , (MReturn, Arity 0)
  , (MSkip, Arity 0)
  , (MSkipN, Arity 1)
  , (MCond, Arity 1)
  , (MNumIn, Arity 0)
  , (MCharIn, Arity 0)
  , (MStrIn, Arity 0)
  , (MNumOut, Arity 1)
  , (MCharOut, Arity 1)
  ]
