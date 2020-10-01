-- | The lexical structure of an Agda file.
--
-- We parse nested block comments.
--
-- If a block comment of nesting level 0 starts ("{-") at the
-- beginning of a line, it is considered "commenting out".  We then
-- parse until we are back to nesting level 0 and return a Comment.
--
-- If not in a block comment a line starts with "--" it is considered
-- a line of text.
--
-- A line consisting only of whitespace is a blank line.
--
-- Everything else is Agda code.  If it contains a block comment start,
-- we parse until we are back at nesting level 0.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module LexicalStructure
  ( parseItems
  , Item
  , pattern CodeItem
  , pattern CommItem
  , pattern TextItem
  ) where

import Data.Bifunctor
import Data.Char (isSpace)
import Data.Maybe

-- * Item parser

-- | We recognize three different kinds of content in Agda files.
data Ty
  = Code    -- ^ Agda code.
  | Text    -- ^ Single line comments, to be converted to text.
  | Comment -- ^ Commented out parts in block comments.
  deriving (Show, Eq)

-- | The result of parsing is a list of items.
type Item = Item' String

data Item' a = Item Ty a
  deriving (Show, Functor)

-- | Agda code.
pattern CodeItem a = Item Code a

-- | Text from consecutive single line comments.
pattern TextItem a = Item Text a

-- | Commented out code from block comments.
pattern CommItem a = Item Comment a

-- | Entry point for the parser of the lexical structure.

parseItems
  :: RevList Item  -- ^ Accumulator, reversed list.
  -> String        -- ^ Input.
  -> [Item]        -- ^ Parse result.
parseItems acc = \case
  [] -> reverse acc
  s  ->
    case parseItem 0 Nothing s of
      (mi, s') -> parseItems (maybeToList mi ++ acc) s'

type RevString = String
type NumBlankLines = Int

-- | Type synonym for list to mark it as snoc-list.
type RevList a = [a]
type State = Maybe (Item' (RevList String))

-- | Parse one item.

parseItem
  :: NumBlankLines -- ^ Parser state part 1.
  -> State         -- ^ Parser state part 2: what are we parsing?
  -> String        -- ^ Not empty when called from outside.
  -> (Maybe Item, String)
parseItem n it s =
  -- We discard trailing blank lines.
  if null s then done

  -- Peek at next line
  else case (it, line) of

      (_, Nothing)   -> parseItem (n + 1) it s'

      -- Start parsing an item.  We discard initial blank lines.
      (Nothing, _)   -> parseItem 0 (lineToState line) s'

      (Just (Item t ls), Just (Item t' (k, l)))
        | t /= t'   -> done
        | otherwise -> parseItem 0 (Just $ Item t ls') s'
            where ls' = indent k l : replicate n "" ++ ls

  where
  (line, s') = parseLine 0 s
  stateToResult = fmap $ fmap $ unlines . reverse
  done = (stateToResult it, s)

-- * Line parser

type Line = Maybe (Item' (Indentation, String))

-- | Just whitespace.
pattern BlankLine = Nothing

-- | Agda code.
pattern CodeLine n s = Just (CodeItem (n, s))

-- | Single line comment.
pattern TextLine n s = Just (TextItem (n, s))

-- | Commented out.
pattern CommentedOut s = Just (CommItem (0, s))

type Indentation  = Int

lineToState :: Line -> State
lineToState = fmap $ fmap $ (:[]) . uncurry indent

-- UNUSED:
-- lineToItem :: Line -> Maybe Item
-- lineToItem = fmap $ fmap $ uncurry indent

-- lineToItem :: Line -> Maybe Item
-- lineToItem = \case
--   BlankLine      -> Nothing
--   CodeLine n s   -> CodeItem $ indent n s
--   TextLine n s   -> TextItem $ indent n s
--   CommentedOut s -> Comment s

indent :: Indentation -> String -> String
indent n s = replicate n ' ' ++ s

type RespectBlockComment = Bool
type NestingLevel = Int

-- | Parse next line (including block comment if it starts on that line).

parseLine :: Indentation -> String -> (Line, String)
parseLine ind = \case

  -- End of line/file

  []          -> (BlankLine, [])
  '\r':'\n':s -> (BlankLine, s)
  '\r':s      -> (BlankLine, s)
  '\n':s      -> (BlankLine, s)

  -- Space
  ' ':s     -> parseLine (ind + 1) s

  -- -- Pragma
  -- s@('{':'-':'#':_) -> first (CodeLine ind . reverse) $ parseToEOL True [] s

  -- Commenting out
  '{':'-':s | ind == 0, take 1 s /= "#"
            -> first (CommentedOut . dropOneSpace . reverse . trimLeft) $
                 parseBlockComment False 0 [] s

  -- Line comment to text; ingore block comments here
  '-':'-':s -> first (TextLine ind . dropOneSpace . reverse . trimLeft) $
                 parseToEOL False [] s

  -- Other: Agda code
  s         -> first (CodeLine ind . reverse . trimLeft) $
                 parseToEOL True [] s
  where
  dropOneSpace = \case
    ' ':x -> x
    x     -> x


-- | Parse to the end of the line (unless block comment opens; then include it).

parseToEOL
  :: RespectBlockComment
     -- ^ How should we treat a block commenting opening?
     --
     --   If 'False', ignore it and treat it as ordinary text
     --   (e.g. if inside a line comment).
     --
     --   If 'True', respect it: look for the matching closing
     --   and continue after that.
     --
     --   In any case, do not discard the delimiter(s)!
  -> RevString
  -> String
  -> (RevString, String)
parseToEOL b acc = \case

  -- Reached end of line (EOL).
  []            -> (acc, [])
  '\r':'\n':s   -> (acc, s)
  '\r':s        -> (acc, s)
  '\n':s        -> (acc, s)

  -- Skip over block comment.
  '{':'-':s | b -> uncurry (parseToEOL b) $
                     parseBlockComment True 0 ('-':'{':acc) s

  -- Skip over character.
  c:s           -> parseToEOL b (c:acc) s

-- | Skip over nested block comments.

parseBlockComment
  :: Bool
     -- ^ Include final closing block delimiter?
  -> NestingLevel
  -> RevString
     -- ^ Accumulator, reversed.
  -> String
     -- ^ Input
  -> (RevString, String)
     -- ^ Block comment, reversed, containing inner comment delimiters and line breaks; output.
parseBlockComment b n acc = \case

  -- Close comment.
  '-':'}':s
   | n <= 0    -> (if b then '}':'-':acc else acc, s)
   | otherwise -> parseBlockComment b (n - 1) ('}':'-':acc) s

  -- Open comment.
  '{':'-':s    -> parseBlockComment b (n + 1) ('-':'{':acc) s

  []  -> (acc,[])
  c:s -> parseBlockComment b n (c:acc) s


-- * Auxiliary functions.

trimLeft :: String -> String
trimLeft = dropWhile isSpace
