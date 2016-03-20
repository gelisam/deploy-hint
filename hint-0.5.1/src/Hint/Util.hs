module Hint.Util where

import Data.Char

type Expr = String

-- @safeBndFor expr@ generates a name @e@ such that it does not
-- occur free in @expr@ and, thus, it is safe to write something
-- like @e = expr@ (otherwise, it will get accidentally bound).
-- This ought to do the trick: observe that @safeBndFor expr@
-- contains more digits than @expr@ and, thus, cannot occur inside
-- @expr@.
safeBndFor :: Expr -> String
safeBndFor expr = "e_1" ++ filter isDigit expr

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition prop = foldr (select prop) ([],[])
    where select p x ~(ts,fs) | p x       = (x:ts,fs)
                              | otherwise = (ts, x:fs)

partitionEither :: [Either a b] -> ([a],[b])
partitionEither [] = ([],[])
partitionEither (Left  a:xs) = let (ls,rs) = partitionEither xs in (a:ls,rs)
partitionEither (Right b:xs) = let (ls,rs) = partitionEither xs in (ls,b:rs)

infixr 1 >=>
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \x -> f x >>= g

quote :: String -> String
quote s = concat ["'", s, "'"]
