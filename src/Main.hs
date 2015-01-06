-- Copyright 2015 Mitchell Kember. Subject to the MIT License.

module Main where

import Control.Monad (foldM)
import Data.Map (empty)
import System.Environment (getArgs)

import Lam.Repl (loadFile, startRepl)

-- | Loads all the files specified in the command line arguments, then starts
-- the read-eval-print loop with the augmented environment.
main :: IO ()
main = getArgs >>= foldM (flip loadFile) empty >>= startRepl
