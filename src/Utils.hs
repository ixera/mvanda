module Utils where

import Data.Char (ord)

ords :: String -> [Rational]
ords = map (toRational . ord)

-- tiny bit more efficient version of
-- \a b -> reverse a ++ b
revConcat :: [a] -> [a] -> [a]
revConcat []     ys = ys
revConcat (x:xs) ys = xs `revConcat` (x:ys)