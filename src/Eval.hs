module Eval (eval) where

import Control.Monad (void)

import Utils
import Types
import Instructions (instr)

eval :: Mvanda -> IO ()
eval = void . eval' []

eval' :: Stack -> Mvanda -> IO (Special, Stack)
eval' s (MvAtom a)   = instr a s
eval' s (MvString x) = return (None, ords x `revConcat` s)
eval' s (MvList xs)  = runList s xs
eval' s (MvNum n)    = return (None, n : s)

runList :: Stack -> [Mvanda] -> IO (Special, Stack)
runList s xs = go s xs
  where
    go st []     = return (None, st)
    go st (m:ms) = do
      (sp, st') <- eval' st m
      case sp of
        None    -> go st' ms
        Skip n  -> go st' $ drop n ms
        Recurse -> do
          (_, st'') <- go st' xs
          go st'' ms
        Return  -> return (None, st')