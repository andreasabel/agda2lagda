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

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath  (takeDirectory)
import System.IO        (hPutStr, stderr)
import System.Exit      (exitFailure)

import LexicalStructure
import Options
import Render

main :: IO ()
main = do

  -- Parse options.

  opts@Options{..} <- options
  target@(language, format, mout) <- getTarget opts
  chat opts $ vocalizeOptions opts target

  -- Parse input.

  chat opts $ "Reading input...\n"
  items <- parseItems [] <$> maybe getContents readFile optInput

  -- Generate output.

  let result = case format of
        LaTeX    -> lagdaTex items
        Markdown -> lagdaMd language items

  -- Save to file or print to stdout.

  chat opts $ "Writing output...\n"
  case mout of
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
