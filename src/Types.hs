module Types where

import Control.Monad.State

type Stack = [Rational]

data Special =
    None
  | Skip Int
  | Recurse
  | Return

data Mvanda =
    MvAtom String
  | MvString String
  | MvList [Mvanda]
  | MvNum Rational

instance Show Mvanda where
  show (MvAtom x)   = x
  show (MvString x) = show x
  show (MvNum  x)   = show x
  show (MvList xs)  = "[" ++ s ++ "]"
    where s = unwords $ show <$> xs