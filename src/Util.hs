-- | General-purpose utilities and standard imports.

module Util (module Util, module X) where

import Data.Bifunctor as X
import Data.Char      as X (isSpace)
import Data.Function  as X (on)
import Data.List      as X (dropWhileEnd, groupBy)
import Data.Maybe     as X

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

-- * List manipulation

-- | Update the last element of a list.

updateLast :: (a -> a) -> [a] -> [a]
updateLast _ []     = []
updateLast f (a:as) = loop a as
  where
  loop b []       = [f b]
  loop b (c : cs) = b : loop c cs

-- | Precondition: list non-empty.

initLast :: [a] -> ([a], a)
initLast as = (init as, last as)

-- * String manipulation

-- | Delete whitespace from both ends.

trim :: String -> String
trim = trimRight . trimLeft

-- | Trim whitespace from the front.

trimLeft :: String -> String
trimLeft = dropWhile isSpace

-- | Trim whitespace from the rear.

trimRight :: String -> String
trimRight = dropWhileEnd isSpace
