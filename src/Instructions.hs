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

instructions :: Map String MvInstr
instructions = M.fromList
  [ ("+", MAdd)
  , ("*", MMult)
  , ("-", MSub)
  , ("/", MDiv)
  , ("%", MMod)
  , ("fl", MFloor)
  , ("ce", MCeil)
  , ("ro", MRound)
  , ("_", MNegate)
  , ("<", MLT)
  , (">", MGT)
  , ("=", MEQ)
  , ("!", MNot)
  , (":", MDup)
  , ("$", MSwap)
  , ("@", MDrop)
  , ("rotu", MRotU)
  , ("rotd", MRotD)
  , ("rev", MReverse)
  , ("len", MLength)
  , (".", MRecurse)
  , (";", MReturn)
  , ("~", MSkip)
  , ("skip", MSkipN)
  , ("?", MCond)
  , ("in", MNumIn)
  , ("ic", MCharIn)
  , ("is", MStrIn)
  , ("pn", MNumOut)
  , ("pc", MCharOut)
  ]

exec :: MvInstr -> Stack -> IO (Special, Stack)

exec MAdd  st = binOp (+) st
exec MMult st = binOp (*) st
exec MSub  st = binOp (-) st
exec MDiv  st = binOp (/) st
exec MMod  st = binOp mod' st

exec MFloor  [x] = noop [toRational (floor x)]
exec MCeil   [x] = noop [toRational (ceiling x)]
exec MRound  [x] = noop [toRational (round x)]
exec MNegate [x] = noop [-x]

exec MLT st   = boolBinOp (<) st 
exec MGT st   = boolBinOp (>) st
exec MEQ st   = boolBinOp (==) st
exec MNot [x] = noop $ [n]
  where n = if x == 0 then 1 else 0

exec MDup  [x]    = noop [x, x]
exec MSwap [x, y] = noop [y, x]
exec MDrop _      = noop []

exec MRotU    st = noop $ tail st   ++ [head st]
exec MRotD    st = noop $ [last st] ++ init st
exec MReverse st = noop $ reverse st
exec MLength  st = noop $ length' st : st
  where length' = toRational . length

exec MRecurse _ = return (Recurse, [])
exec MReturn  _ = return (Return, [])
exec MSkip    _ = return (Skip 1, [])
exec MSkipN [n] = return (Skip (floor n), [])
exec MCond 0    = return (Skip 1, [])
exec MCond _    = return (None, [])

exec MCharIn _ = do
  i <- ord <$> getChar
  return (None, [toRational i])

exec MNumIn _ = do
  i <- getLine
  case readMaybe i :: Maybe Integer of
    Nothing -> errorWithoutStackTrace "Invalid integer literal!"
    Just n  -> return (None, [toRational n])

exec MStrIn st = do
  i <- getLine
  return (None, reverse (ords i))

exec MNumOut [x] = do
  let x' = fromRational x :: Double
  if x' `mod'` 1 == 0 then
    putStr $ show $ floor x
  else
    print $ show x'
  hFlush stdout
  return (None, [])

exec MCharOut [x] = do
  let x' = chr (floor x)
  putStr [x']
  hFlush stdout
  return (None, [])

exec _ _ = error'

binOp :: (Rational -> Rational -> Rational) -> Stack -> IO (Special, Stack)
binOp f [x, y] = noop $ [f y x]
binOp _ _      = error'

boolBinOp :: (Rational -> Rational -> Bool) -> Stack -> IO (Special, Stack)
boolBinOp f (x:y:st) = noop $ n : st
  where n = if f y x then 1 else 0
boolBinOp _ _        = error'

noop :: Stack -> IO (Special, Stack)
noop s = return (None, s)

error' :: String -> a
error' = errorWithoutStackTrace "You should never see this message."