import System.Exit     ( exitWith  )
import System.Process  ( system    )
import System.FilePath ( (</>)     )

import Paths_agda2lagda ( getBinDir )

main :: IO ()
main = do
  bindir <- getBinDir
  exitWith =<< system (bindir </> "goldplate test")
