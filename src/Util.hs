-- | General-purpose utilities.

module Util where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- | Update the last element of a list.

updateLast :: (a -> a) -> [a] -> [a]
updateLast _ []     = []
updateLast f (a:as) = loop a as
  where
  loop b []       = [f b]
  loop b (c : cs) = b : loop c cs

-- | Delete whitespace from both ends.

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace
