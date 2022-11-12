module Options
  ( Options(..)
  , options
  , getTarget
  , self
  , vocalizeOptions
  )
  where

import qualified Data.List as List

import Options.Applicative
import Options.Applicative.Help.Pretty (vcat, text) -- , (<$$>))

import System.Directory (doesDirectoryExist)
import System.FilePath
  (splitExtension, addExtension, takeFileName, hasTrailingPathSeparator, (</>))

import Util
import Version

self :: String
self = "agda2lagda"

extensionMap :: [(String,String)]
extensionMap =
  [ (".agda" , ".lagda.tex")
  , (".hs"   , ".lhs")
  ]

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
