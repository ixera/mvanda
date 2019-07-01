module Instructions (exec, arities) where

import Data.Char (chr, ord)
import Data.Fixed (mod')
import Data.Functor (($>))
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Types
import Utils

arities :: Map MvInstr Arity
arities = M.fromAscList
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

exec :: MvInstr -> Stack -> IO (Special, Stack)

exec "*" st = binOp (*) st
exec "+" st = binOp (+) st
exec "-" st = binOp (-) st
exec "/" st = binOp (/) st
exec "%" st = binOp mod' st

exec "fl" (x:st) = noop $ toRational (floor x)   : st
exec "ce" (x:st) = noop $ toRational (ceiling x) : st
exec "ro" (x:st) = noop $ toRational (round x)   : st
exec "_" (x:st)  = noop $ (-x) : st

exec "<" st     = boolBinOp (<) st 
exec ">" st     = boolBinOp (>) st
exec "=" st     = boolBinOp (==) st
exec "!" (x:st) = noop $ n : st
  where n = if x == 0 then 1 else 0

exec ":" (x:st)   = noop $ x : x : st
exec "$" (x:y:st) = noop $ y : x : st
exec "@" (_:st)   = noop $ st

exec "rotu" st    = noop $ tail st   ++ [head st]
exec "rotd" st    = noop $ [last st] ++ init st
exec "rev" st     = noop $ reverse st
exec "len" st     = noop $ length' st : st
  where length' = toRational . length

exec "." st        = return (Recurse, st)
exec ";" st        = return (Return, st)
exec "~" st        = return (Skip 1, st)
exec "skip" (x:st) = return (Skip (floor x), st)
exec "?" (0:st)    = return (Skip 1, st)
exec "?" (_:st)    = noop st

exec "ic" st = do
  i <- ord <$> getChar
  return (None, toRational i : st)

exec "in" st = do
  i <- getLine
  case readMaybe i :: Maybe Integer of
    Nothing -> error' "Invalid integer literal!"
    Just n  -> return (None, toRational n : st)

exec "is" st = do
  i <- getLine
  return (None, ords i `revConcat` st)

exec "pn" (x:st) = do
  let x' = fromRational x :: Double
  if x' `mod'` 1 == 0 then
    putStr $ show $ floor x
  else
    print $ show x'
  hFlush stdout
  return (None, st)

exec "pc" (x:st) = do
  let x' = chr (floor x)
  putStr [x']
  hFlush stdout
  return (None, st)

exec i _ = error' $ "Unknown instruction `" ++ i ++ "`!"

binOp :: (Rational -> Rational -> Rational) -> Stack -> IO (Special, Stack)
binOp f (x:y:st) = noop $ f y x : st
binOp _ _        = error' "Not enough items on stack!"

boolBinOp :: (Rational -> Rational -> Bool) -> Stack -> IO (Special, Stack)
boolBinOp f (x:y:st) = noop $ n : st
  where n = if f y x then 1 else 0
boolBinOp _ _        = error' "Not enough items on stack!"

noop :: Stack -> IO (Special, Stack)
noop s = return (None, s)

error' :: String -> a
error' = errorWithoutStackTrace