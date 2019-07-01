module Types where

import Control.Monad.State

type Stack = [Rational]

data Special =
    None
  | Skip Int
  | Recurse
  | Return

data MvInstr =
    MNoop
  | MMult
  | MAdd
  | MSub
  | MDiv
  | MMod
  | MFloor
  | MCeil
  | MRound
  | MNegate
  | MLT
  | MGT
  | MEQ
  | MNot
  | MDup
  | MSwap
  | MDrop
  | MRotU
  | MRotD
  | MReverse
  | MLength
  | MRecurse
  | MReturn
  | MSkip
  | MSkipN
  | MCond
  | MNumIn
  | MCharIn
  | MStrIn
  | MNumOut
  | MCharOut
  deriving (Eq, Ord)
  -- Ord is required for the Map in Arities.hs
  -- Eq is required for Ord

data Arity =
    Arity Int
  | Stack

data Mvanda =
    MvInstr MvInstr String
  | MvString String
  | MvBlock [Mvanda]
  | MvNum Rational

instance Show Mvanda where
  show (MvInstr x s) = s
  show (MvString x)  = show x
  show (MvNum  x)    = show x
  show (MvBlock xs)  = "[" ++ s ++ "]"
    where s = unwords $ show <$> xs