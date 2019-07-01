module Instructions (instr) where

import Data.Char (chr, ord)
import Data.Fixed (mod')
import Data.Functor (($>))
import Text.Read (readMaybe)
import System.IO (hFlush, stdout)

import Types
import Utils

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