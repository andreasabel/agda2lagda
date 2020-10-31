-- Sample non-literate Agda program
-- --------------------------------
--
-- This file serves as example for agda2lagda.
-- The content may be non-sensical.

module Foo where

-- Some data type.

data D : Set where
  c : D

-- A function.
foo : D → D
foo c = c   -- basically, the identity

{- This part is commented out.
{-
bar : D → Set
bar x = D
-- -}
-- -}

-- Another heading
------------------

module Submodule where

  postulate
    zeta : D

-- That's it.
-- Bye.
