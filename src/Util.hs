-- | General-purpose utilities.

module Util where

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- | Update the last element of a list.

updateLast :: (a -> a) -> [a] -> [a]
updateLast _ []     = []
updateLast f (a:as) = loop a as
  where
  loop b []       = [f b]
  loop b (c : cs) = b : loop c cs
