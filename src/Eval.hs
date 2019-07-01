module Eval (eval) where

import Control.Monad (void)
import Data.Map.Strict ((!))

import Utils
import Types
import Instructions (exec, arities)

eval :: Mvanda -> IO ()
eval = void . eval' []

eval' :: Stack -> Mvanda -> IO (Special, Stack)
eval' st (MvInstr i s) = case arities ! i of
  Stack ->
    exec i st
  Arity n ->
  if length st < n then
    errorWithoutStackTrace $
      "Error in instruction `" ++ s ++ "`, not enough items on stack."
  else do
    let (pop, rest) = splitAt n
    (sp, xs) <- exec i pop
    return (sp, xs ++ rest)
eval' st (MvString x)  = return (None, ords x `revConcat` st)
eval' st (MvBlock xs)  = runBlock st xs
eval' st (MvNum n)     = return (None, n : st)

runBlock :: Stack -> [Mvanda] -> IO (Special, Stack)
runBlock s xs = go s xs
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