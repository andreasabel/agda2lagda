-- | Render the parsed items.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Render
  ( lagdaTex
  ) where

import qualified LexicalStructure as L
import Markup
import Version

-- | Render into plain literate LaTeX Agda.

lagdaTex :: [L.Item] -> String
lagdaTex its = unlines $ concat
   [ ["%% This file was automatically generated by agda2lagda " ++ version ++ "."]
   , [""]
   , map render $ markup its
   ]
  where
  render = \case
    TextItem it -> renderTex it
    CommItem s  -> unlines $ map ("%% " ++) $ lines s
    CodeItem s  -> concat
      [ "\\begin{code}\n"
      , s
      , "\\end{code}\n"
      ]

-- | Render a markup item

renderTex :: TextItem -> String
renderTex = \case
  Paragraph p -> unlines p
  Heading 1 s -> "\\heading{" ++ s ++ "}\n"
  Heading 2 s -> "\\subheading{" ++ s ++ "}\n"
  Itemize ps  -> unlines $ concat $
    [ [ "\\begin{itemize}\n" ]
    , map (unlines . ("\\item" :)) ps
    , [ "\\end{itemize}" ]
    ]
