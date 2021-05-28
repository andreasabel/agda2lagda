-- | agda2lagda
--
-- Translate line comments into ordinary text,
-- block comments into (LaTeX) comments,
-- and wrap regular Agda code into code delimiters (\begin{code}...\end{code}).

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad
import qualified Data.List as List
import Data.Semigroup

import Options.Applicative
import Options.Applicative.Help.Pretty (vcat, text) -- , (<$$>))

import System.Directory (doesFileExist, doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath
  (splitExtension, addExtension, takeFileName, takeDirectory, hasTrailingPathSeparator, (</>))
import System.IO        (hPutStr, stderr)
import System.Exit      (exitFailure)

import LexicalStructure
import Render
import Util             ((<&>))
import Version

self :: String
self = "agda2lagda"

extensionMap :: [(String,String)]
extensionMap =
  [ (".agda" , ".lagda.tex")
  , (".hs"   , ".lhs")
  ]

main :: IO ()
main = do

  -- Parse options.

  opts@Options{..} <- options
  target <- getTarget opts
  chat opts $ vocalizeOptions opts target

  -- Parse input.

  chat opts $ "Reading input...\n"
  items <- parseItems [] <$> maybe getContents readFile optInput

  -- Generate output.

  let result = lagdaTex items

  -- Save to file or print to stdout.

  chat opts $ "Writing output...\n"
  case target of
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

      createDirectoryIfMissing True $ takeDirectory outFile
      writeFile outFile result

  -- Done.

  chat opts $ unwords [ self, "terminated successfully.\n" ]

-- * Option parsing and handling

data Options = Options
  { optVerbose    :: Bool
  , optDryRun     :: Bool
  , optForce      :: Bool
  , optOutput     :: Maybe FilePath
  , optInput      :: Maybe FilePath
  } deriving Show

options :: IO Options
options =
  execParser $
    info (helper <*> versionOption <*> numericVersionOption <*> programOptions)
         (header "Translates Agda/Haskell text into literate Agda/Haskell text, turning line comments into ordinary text and code into TeX code blocks."
          <> footerDoc (Just foot))

  where
  versionOption =
    infoOption (unwords versionWords)
      $  long "version"
      <> help "Show version info."
  versionWords = concat
    [ [ self, "version", version ]
    , [ "(Halloween edition)" | ".11.1" `List.isSuffixOf` version ]
    ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> help "Show just version number."
      -- Newline at the end:
      -- <> helpDoc (Just $ text "Show just version number." <$$> text "")

  programOptions = Options
    <$> oVerbose
    <*> oDryRun
    <*> oForce
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
      <> metavar "OUT"
      <> action "directory"
      <> help "Name of output file or directory."

  oStdin =
    flag' Nothing
      $  long "stdin"
      <> help "Read from standard input."

  oInput :: Parser (Maybe FilePath)
  oInput = dashToStdin <$> do
    strArgument
      $  metavar "INFILE"
      <> action "file"
      <> help "The input file containing the Agda/Haskell text."
    where
    -- dash is interpreted as stdin
    dashToStdin = \case
          "-"  -> Nothing
          file -> Just file

  foot = vcat $ map text $ concat
    [ [ "Unless explicitly given via -o, the name of the output file is computed by replacing the extension of the input file, according to the following rules:"
      , "" ]
    , flip map extensionMap $ \ (src, tgt) ->
        List.intercalate "\t" [ "", src, "-->", tgt ]
    , [ ""
      , "If the path OUT given via -o is a directory, that's where the output file will be placed."
      , ""
      , unwords [ "Example:", self, "path/to/file.agda" ]
      , ""
      , "This will write file path/to/file.lagda.tex unless it already exists."
      , "To overwrite existing files, use option -f."
      , ""
      , unwords [ "Example:", self, "path/to/file.hs", "-o out/dir" ]
      , ""
      , "This places the output in file out/dir/file.lhs."
      , ""
      , unwords [ "Example:", self, "path/to/file.hs", "-o new/dir/" ]
      , ""
      , "This places the output in file new/dir/file.lhs, creating directories new/ and new/dir/ if necessary."
      ]
    ]

type Target = Maybe FilePath

vocalizeOptions :: Options -> Target -> String
vocalizeOptions Options{..} target = unlines $ map concat
  [ [ "Plan:" ]
  , [ "- Read from "
    , maybe "standard input" ("file " ++) optInput
    , "."
    ]
  , [ "- Write to "
    , maybe "standard output" (\ f -> "file " ++ f ++ unlessNewer) target
    , "."
    ]
  ]
  where
  unlessNewer
    | optForce  = ""
    | otherwise = " (only if output is not newer than input)"

getTarget :: Options -> IO Target
getTarget Options{..}
  | optDryRun = return Nothing
  | otherwise = traverse dirOrFile optOutput <&> \ mout ->
      if | Just (File out) <- mout -> Just out
         | Just inp <- optInput    ->
             case splitExtension inp of
               (base, src) | Just tgt <- lookup src extensionMap
                 -> Just $ addDir mout $ addExtension base tgt
               _ -> Nothing
         | otherwise -> Nothing
      where
      addDir (Just (Dir dir)) = (dir </>) . takeFileName
      addDir _ = id

data DirOrFile = Dir FilePath | File FilePath

dirOrFile :: FilePath -> IO DirOrFile
dirOrFile path
  | hasTrailingPathSeparator path = return $ Dir path
  | otherwise = doesDirectoryExist path <&> \case
      True  -> Dir path
      False -> File path

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
