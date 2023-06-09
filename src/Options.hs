module Options
  ( Options(..)
  , options
  , Format(..)
  , Language(..)
  , languageToHighlighter
  , getTarget
  , self
  , vocalizeOptions
  )
  where

import qualified Data.List as List

import Options.Applicative
import Options.Applicative.Help.Pretty (vcat, pretty) -- , (<$$>))

import System.Directory (doesDirectoryExist)
import System.FilePath
  (splitExtension, addExtension, takeFileName, hasTrailingPathSeparator, (</>))

import Util
import Version

-- import Debug.Trace (traceShowId)

self :: String
self = "agda2lagda"

extensionMap :: [(String,(Language,[(Format,String)]))]
extensionMap =
  [ (".agda" , (Agda    , [ (LaTeX    , ".lagda.tex")
                          , (Markdown , ".lagda.md" ) ]))
  , (".hs"   , (Haskell , [ (LaTeX    , ".lhs"      )
                          , (Markdown , ".lhs"      ) ]))
  ]

-- | Map extensions to formats.
formatMap :: [(String, Format)]
formatMap =
  [ (".tex", LaTeX)
  , (".md", Markdown)
  , (".markdown", Markdown)
  ]

-- | From the name of the output file, determine the format.
formatFromFilename :: String -> Maybe Format
formatFromFilename file =
  case filter (\ (ext, _) -> ext `List.isSuffixOf` file) formatMap of
    -- Only determine format if we have exactly one hit
    [ (_, fmt) ] -> Just fmt
    _ -> Nothing

-- | Format of the output.
data Format
  = LaTeX    -- ^ Latex literate text.
  | Markdown -- ^ Markdown literate text.
  deriving (Eq, Show)

-- formatToLongOption :: Format -> String
-- formatToLongOption = map toLower . show

data Language
  = Agda
  | Haskell
  deriving (Eq, Show)

-- | Get markdown highlighter identifier (e.g. "agda", "haskell") for language.
languageToHighlighter :: Language -> String
languageToHighlighter = map toLower . show

-- * Option parsing and handling

data Options = Options
  { optVerbose    :: Bool
  , optDryRun     :: Bool
  , optForce      :: Bool
  , optFormat     :: Maybe Format
  -- , optLatex      :: Bool
  -- , optMarkdown   :: Bool
  , optOutput     :: Maybe FilePath
  , optInput      :: Maybe FilePath
  } deriving Show

inferFormatFromOutput :: Options -> Options
inferFormatFromOutput opts
  | Nothing <- optFormat opts, Just out <- optOutput opts = opts { optFormat = formatFromFilename out }
  | otherwise = opts

-- | Default format to 'LaTeX'.
getFormat :: Options -> Format
getFormat = fromMaybe LaTeX . optFormat

options :: IO Options
options = inferFormatFromOutput <$> do
  execParser $
    info (helper <*> versionOption <*> numericVersionOption <*> programOptions)
         (header "Translates Agda/Haskell text into literate Agda/Haskell text, turning line comments into ordinary text and code into TeX code blocks."
          <> footerDoc (Just foot))

  where
  versionOption =
    infoOption (unwords versionWords)
      $  long "version"
      <> short 'V'
      <> hidden
      <> help "Show version info."
  versionWords = concat
    [ [ self, "version", version ]
    , [ "(Halloween edition)" | ".11.1" `List.isSuffixOf` version ]
    ]

  numericVersionOption =
    infoOption version
      $  long "numeric-version"
      <> hidden
      <> help "Show just version number."
      -- Newline at the end:
      -- <> helpDoc (Just $ pretty "Show just version number." <$$> pretty "")

  programOptions = Options
    <$> oVerbose
    <*> oDryRun
    <*> oForce
    <*> oFormat
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
      <> hidden
      <> help "Do not write any output files, write to standard output."

  oVerbose =
    switch
      $  long "verbose"
      <> short 'v'
      <> hidden
      <> help "Comment on what is happening."

  oFormat =
    flag Nothing (Just Markdown)
      $  long "markdown"
      <> help "Produce Markdown instead of LaTeX."

  -- oLaTeX =
  --   switch
  --     $  long (formatToLongOption LaTeX)
  --     <> help (concat ["Generate LaTeX (default if --", formatToLongOption Markdown, " is not given)."])

  -- oMarkdown =
  --   switch
  --     $  long (formatToLongOption Markdown)
  --     <> help "Generate Markdown (possibly inferred from OUT file extension)."

  -- oLaTeX =
  --   optional $ strOption
  --     $  long "latex"
  --     <> value ""
  --     <> noArgError (ShowHelpText Nothing)
  --     <> metavar "LATEXFILE"
  --     <> action "file"
  --     <> help "Generate LaTeX (if LATEXFILE is not given, it is computed from INFILE and OUT)."

  oOutput =
    optional $ strOption
      $  long "output"
      <> short 'o'
      <> metavar "OUT"
      <> action "directory"
      <> help "Name of output file or directory.  Ending '.md' or '.markdown' determines Markdown output."

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

  foot = vcat $ map pretty $ concat
    [ [ "Unless explicitly given via -o, the name of the output file is computed by replacing the extension of the input file, according to the following rules:"
      , "" ]
    , extensionMap >>= \ (src, (_language, rest)) ->
        rest <&> \ (format, tgt) ->
        -- let o = "--" ++ formatToLongOption format ++ ":" in
        let o = show format in
        List.intercalate "\t" [ "", src, "--" ++ o  ++ "-->", tgt ]
    , [ ""
      , "If the path OUT given via -o ends in '.md' or '.markdown', '--markdown' is assumed."
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

type Target = (Language, Format, Maybe FilePath)

vocalizeOptions :: Options -> Target -> String
vocalizeOptions Options{..} (language, format, target) = unlines $ map concat
  [ [ "Plan:" ]
  , [ "- Read " ++ show language ++ " code from "
    , maybe "standard input" ("file " ++) optInput
    , "."
    ]
  , [ "- Write " ++ show format ++ " to "
    , maybe "standard output" (\ f -> "file " ++ f ++ unlessNewer) target
    , "."
    ]
  ]
  where
  unlessNewer
    | optForce  = ""
    | otherwise = " (only if output is not newer than input)"

getTarget :: Options -> IO Target
getTarget opts@Options{..} = do
  let language = maybe Agda fst m
  outFile <- if optDryRun then return Nothing else do
    traverse dirOrFile optOutput <&> \ mout ->
      if | Just (File out) <- mout    -> Just out
         | Just (_, (base, tgt)) <- m -> Just $ addDir mout $ addExtension base tgt
         | otherwise -> Nothing
  return (language, format, outFile)
  where
    format = getFormat opts
    -- Resolve Language, basename, and extension from input file.
    m :: Maybe (Language, (FilePath, FilePath))
    m = do
      inp <- optInput
      let (base, src) = splitExtension inp
      (language, rest) <- lookup src extensionMap
      ext <- lookup format rest
      Just (language, (base, ext))

    addDir (Just (Dir dir)) = (dir </>) . takeFileName
    addDir _ = id

data DirOrFile = Dir FilePath | File FilePath

dirOrFile :: FilePath -> IO DirOrFile
dirOrFile path
  | hasTrailingPathSeparator path = return $ Dir path
  | otherwise = doesDirectoryExist path <&> \case
      True  -> Dir path
      False -> File path
