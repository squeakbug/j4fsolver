module Main ( main ) where

import           Language.Haskell.HLint ( hlint )
import           System.Exit            ( exitFailure, exitSuccess )

arguments :: [String]
arguments = [ "src", "test", "examples" ]

main :: IO ()
main = do
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure