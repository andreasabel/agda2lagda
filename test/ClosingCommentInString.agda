-- This demonstrates a known issue:
-- The parser is not aware of strings, thus,
-- a closing block comment delimiter inside a string
-- is taken as a closing block comment delimiter.

-- Without nested comments:

{- This is commented-out.
test = "A weird string with comment closing -} and other garbage @#$% that appears now as Agda code!"
-}

-- With nested comments:

{-
{- This is commented-out.
test = "A weird string with comment closing -} and other garbage @#$%!"
-- NB: this confuses even Agda (at the time of writing, version 2.6.1).
-}
This should be still commented-out. -}

-- The reverse situation:

test = "Never put comment opening {- inside a string!"

postulate
  ThisShouldNotBeCommentedOut : Set

-- -} This should be text, not code!!

-- SNAFU.
