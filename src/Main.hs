-- | agda2lagda
--
-- Translate line comments into ordinary text,
-- block comments into (LaTeX) comments,
-- and wrap regular Agda code into code delimiters (\begin{code}...\end{code}).

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import Data.Semigroup
import Options.Applicative

import System.Directory (doesFileExist) -- getModificationTime
import System.FilePath  (splitExtension, addExtension)
import System.IO        (hPutStr, stderr)
import System.Exit      (exitFailure)

import Text.PrettyPrint.ANSI.Leijen (vcat, text)

import LexicalStructure
import Render

main :: IO ()
main = do

  -- Parse options.

  opts@Options{..} <- options
  chat opts $ vocalizeOptions opts

  -- Parse input.

  chat opts $ "Reading input...\n"
  items <- parseItems [] <$> maybe getContents readFile optInput

  -- Generate output.

  let result = lagdaTex items

  -- Save to file or print to stdout.

  chat opts $ "Writing output...\n"
  case target opts of
    Nothing      -> putStr result
    Just outFile -> do

      -- Do not overwrite existing file unless forced.
      unless optForce $ do
        ex <- doesFileExist outFile
        when ex $ do
          hPutStr stderr $ unlines
            [ unwords [ "Error: output file", outFile, "already exists." ]
            , "Use option --force to allow to overwrite existing files."
            , "Aborting."
            ]
          exitFailure

      writeFile outFile result

  -- Done.

  chat opts $ "agda2lagda terminated successfully.\n"

data Options = Options
  { optForce      :: Bool
  , optDryRun     :: Bool
  , optVerbose    :: Bool
  , optOutput     :: Maybe FilePath
  , optInput      :: Maybe FilePath
  } deriving Show

options :: IO Options
options =
  execParser $
    info (helper <*> opts)
         (header "Translates Agda text into literate Agda text, turning line comments into text and code into code blocks."
          <> footerDoc (Just foot))

  where
  opts = Options
    <$> oForce
    <*> oDryRun
    <*> oVerbose
    <*> oOutput
    <*> (oStdin <|> oInput)

  oForce =
    switch
      $  long "force"
      <> short 'f'
      <> help "Overwrite existing output files."

  oDryRun =
    switch
      $  long "dry-run"
      <> help "Do not write any output files, write to standard output."

  oVerbose =
    switch
      $  long "verbose"
      <> short 'v'
      <> help "Comment on what is happening."

  oOutput =
    optional $ strOption
      $  long "output"
      <> short 'o'
      <> metavar "OUTFILE"
      <> help "Name of output file."

  oStdin =
    flag' Nothing
      $  long "stdin"
      <> help "Read from standard input"

  oInput :: Parser (Maybe FilePath)
  oInput = dashToStdin <$> do
    strArgument
      $  metavar "INFILE"
      <> help "The input file containing the Agda text."
    where
    -- dash is interpreted as stdin
    dashToStdin = \case
          "-"  -> Nothing
          file -> Just file

  foot = vcat $ map text
    [ "Example: agda2lagda path/to/file.agda"
    , ""
    , "This will write file path/to/file.lagda.tex unless it already exists."
    , "To overwrite existing files, use option -f."
    ]

vocalizeOptions :: Options -> String
vocalizeOptions o@(Options {..}) = unlines $ map concat
  [ [ "Plan:" ]
  , [ "- Read from "
    , maybe "standard input" ("file " ++) optInput
    , "."
    ]
  , [ "- Write to "
    , maybe "standard output" (\ f -> "file " ++ f ++ unlessNewer) $ target o
    , "."
    ]
  ]
  where
  unlessNewer
    | optForce  = ""
    | otherwise = " (only if output is not newer than input)"

target :: Options -> Maybe FilePath
target (Options {..})
  | optDryRun = Nothing
  | Just out <- optOutput = Just out
  | Just inp <- optInput  =
      case splitExtension inp of
        (base, ".agda") -> Just $ addExtension base ".lagda.tex"
        _ -> Nothing
  | otherwise = Nothing

chat :: Options -> String -> IO ()
chat o msg = when (optVerbose o) $ hPutStr stderr msg

-- UNUSED:
-- -- | Checks where first file has been more recently modified than second file.
-- --   If first file does not exist, return 'False'
-- --   (its modification time is then taken to be minus infinity).
-- --
-- --   Precondition: second file exists.
-- newerThan :: FilePath -> FilePath -> IO Bool
-- newerThan f1 f2 = do
--   t2 <- getModificationTime f2
--   ((> t2) <$> getModificationTime t1)
--     `catch` \ e
--        | isPermissionError e || isDoesNotExistError e -> return False
--        | otherwise -> throw e
