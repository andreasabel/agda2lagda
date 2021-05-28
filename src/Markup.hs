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
  = Paragraph Paragraph   -- ^ A non-empty list of non-whitespace lines.
  | Heading Level String  -- ^ A heading at the given level (1,2).
  | Itemize [Paragraph]   -- ^ A bulleted list (non-nested).

type Level = Int

-- | Entrypoint: Process a lexed file.

markup :: [L.Item] -> [Item]
markup = concatMap processText . gobbleTrailingBlockCommentClosers
  where
  processText = \case
    L.CommItem s -> [CommItem s]
    L.CodeItem s -> [CodeItem s]
    L.TextItem s ->
      map TextItem
      . groupBullets
      . map detectMarkup
      $ paragraphs s

-- | A paragraph is a non-empty list of non-empty lines.
type Paragraph = [String]

groupBullets :: [TextItem] -> [TextItem]
groupBullets = map joinItems . groupBy (\ a b -> all (isJust . isItemize) [a,b])
  where
  isItemize = \case
    Itemize ps -> Just ps
    _ -> Nothing
  joinItems :: [TextItem] -> TextItem
  joinItems is =
    -- In each group...
    case mapMaybe isItemize is of
      -- ...either it is not Itemize
      []  -> head is
      -- or all are Itemize.
      pps -> Itemize $ concat pps

-- | If the last line of a paragraph is just dashes or equal signs, we are a heading.

detectMarkup :: Paragraph -> TextItem
detectMarkup ls@(l1:ls1)
  | Just s <- bulleted l1 = Itemize [s:ls1]
  | all (== '=') l0 = Heading 1 $ unwords $ map trimLeft ls0
  | all (== '-') l0 = Heading 2 $ unwords $ map trimLeft ls0
  | otherwise       = Paragraph ls
  where
  (ls0, l) = initLast ls  -- safe!
  l0 = trimLeft l

-- | If the line starts with @*@, return the rest.

bulleted :: String -> Maybe String
bulleted l =
  case trimLeft l of
    '*':' ':s -> Just s
    _ -> Nothing

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
