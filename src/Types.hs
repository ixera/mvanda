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
  deriving Show

data Mvanda =
    MvInstr MvInstr
  | MvString String
  | MvBlock [Mvanda]
  | MvNum Rational

instance Show Mvanda where
  show (MvInstr x)  = show x
  show (MvString x) = show x
  show (MvNum  x)   = show x
  show (MvBlock xs) = "[" ++ s ++ "]"
    where s = unwords $ show <$> xs