-- | Render the parsed items.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Render ( lagdaTex ) where

import LexicalStructure
  (Item, pattern TextItem, pattern CommItem, pattern CodeItem)

-- | Render into plain literate LaTeX Agda.

lagdaTex :: [Item] -> String
lagdaTex = foldr (\ it s -> render it ++ "\n" ++ s) ""
  where
  render = \case
    TextItem s -> s
    CommItem s -> unlines $ map ("%% " ++) $ lines s
    CodeItem s -> concat
      [ "\\begin{code}\n"
      , s
      , "\\end{code}\n"
      ]
