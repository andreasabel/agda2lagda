-- Sample non-literate Agda program
-- ================================
--
-- A remark to test bulleted lists:
--
-- * This file serves as example for agda2lagda.
--
-- * The content may be non-sensical.
--
-- Indeed!

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

-- A subheading
---------------

module Submodule where

  postulate
    zeta : D

-- That's it.
-- Bye.
