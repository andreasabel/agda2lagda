-- | Detect markup in the text items.
--
-- A line of text consisting only of "=" (and at least 2) is a heading.
-- A line of text consisting only of "-" (and at least 2) is a subheading.

module Markup
  ( markup, Item(..), TextItem(..)
  ) where

import Util
import qualified LexicalStructure as L

-- | A more refined version of 'LexicalStructure.Item'.

data Item
  = CodeItem  String  -- ^ Same.
  | CommItem  String  -- ^ Same.
  | TextItem TextItem

data TextItem
  = Paragraph String      -- ^ A paragraph without empty lines in between.
  | Heading Level String  -- ^ A heading at the given level (1,2).

type Level = Int

-- | Entrypoint: Process a lexed file.

markup :: [L.Item] -> [Item]
markup = concatMap processText . gobbleTrailingBlockCommentClosers
  where
  processText = \case
    L.TextItem s -> map (TextItem . detectHeading) $ paragraphs s
    L.CommItem s -> [CommItem s]
    L.CodeItem s -> [CodeItem s]

-- | A paragraph is a non-empty list of non-empty lines.
type Paragraph = [String]

-- | If the last line of a paragraph is just dashes or equal signs, we are a heading.

detectHeading :: Paragraph -> TextItem
detectHeading ls
  | all (== '=') l0 = Heading 1 $ unwords $ map trimLeft ls0
  | all (== '-') l0 = Heading 2 $ unwords $ map trimLeft ls0
  | otherwise       = Paragraph $ unlines ls
  where
  (ls0, l) = initLast ls  -- safe!
  l0 = trimLeft l

-- | Split a text into paragraphs, dropping extra empty lines in between.
--   Preserves indentation.

paragraphs :: String -> [Paragraph]
paragraphs = mapMaybe filterBlank . groupBy emptyOrNot . lines
  where
  emptyLine  = null . trimLeft
  emptyOrNot = (==) `on` emptyLine
  filterBlank ls
    | all emptyLine ls = Nothing
    | otherwise        = Just ls

-- | Text lines at the end of file consisting solely of words "-}" are
-- discarded, to accommodate for a hack that makes it easy to comment
-- out everything to the end of the file.
-- https://github.com/agda/agda/issues/4953#issuecomment-702720296

gobbleTrailingBlockCommentClosers :: [L.Item] -> [L.Item]
gobbleTrailingBlockCommentClosers = updateLast $ \case
  L.TextItem s -> L.TextItem $ reverse $ loop $ reverse s
  i -> i
  where
  loop = \case
    -- Drop trailing spaces.
    c:cs | isSpace c -> loop cs
    -- Drop trailing block comment closers.
    '}':'-':cs -> loop cs
    -- Keep rest.
    s -> s
