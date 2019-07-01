module Instructions (instr) where

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

instr :: MvInstr -> Stack -> IO (Special, Stack)

instr "*" st = binOp (*) st
instr "+" st = binOp (+) st
instr "-" st = binOp (-) st
instr "/" st = binOp (/) st
instr "%" st = binOp mod' st

instr "fl" (x:st) = noop $ toRational (floor x)   : st
instr "ce" (x:st) = noop $ toRational (ceiling x) : st
instr "ro" (x:st) = noop $ toRational (round x)   : st
instr "_" (x:st)  = noop $ (-x) : st

instr "<" st     = boolBinOp (<) st 
instr ">" st     = boolBinOp (>) st
instr "=" st     = boolBinOp (==) st
instr "!" (x:st) = noop $ n : st
  where n = if x == 0 then 1 else 0

instr ":" (x:st)   = noop $ x : x : st
instr "$" (x:y:st) = noop $ y : x : st
instr "@" (_:st)   = noop $ st

instr "rotu" st    = noop $ tail st   ++ [head st]
instr "rotd" st    = noop $ [last st] ++ init st
instr "rev" st     = noop $ reverse st
instr "len" st     = noop $ length' st : st
  where length' = toRational . length

instr "." st        = return (Recurse, st)
instr ";" st        = return (Return, st)
instr "~" st        = return (Skip 1, st)
instr "skip" (x:st) = return (Skip (floor x), st)
instr "?" (0:st)    = return (Skip 1, st)
instr "?" (_:st)    = noop st

instr "ic" st = do
  i <- ord <$> getChar
  return (None, toRational i : st)

instr "in" st = do
  i <- getLine
  case readMaybe i :: Maybe Integer of
    Nothing -> error' "Invalid integer literal!"
    Just n  -> return (None, toRational n : st)

instr "is" st = do
  i <- getLine
  return (None, ords i `revConcat` st)

instr "pn" (x:st) = do
  let x' = fromRational x :: Double
  if x' `mod'` 1 == 0 then
    putStr $ show $ floor x
  else
    print $ show x'
  hFlush stdout
  return (None, st)

instr "pc" (x:st) = do
  let x' = chr (floor x)
  putStr [x']
  hFlush stdout
  return (None, st)

instr i _ = error' $ "Unknown instruction `" ++ i ++ "`!"

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