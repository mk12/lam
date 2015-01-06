-- Copyright 2015 Mitchell Kember. Subject to the MIT License.

-- | Implements the read-eval-print loop in the 'repl' function.
module Lam.Repl (load, repl) where

import Control.Error.Util (maybeT)
import Control.Monad (foldM)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isPrefixOf)
import System.Console.Readline (addHistory, readline)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)

import Lam.Eval
import Lam.Parse

-- | Performs the read-eval-print loop forever: prompts the user, reads input,
-- parses it, evaluates it, prints the result, and repeats. The environment is
-- updated on each iteration with a new binding if the user enters an assignment
-- (and if they only enter an expression, it will still be bound to the percent
-- variable). Also handles special commands for loading files and exiting.
repl :: Environment -> IO ()
repl env = maybe end handle =<< readline "lamb> "
  where
    end = return ()
    again = repl env
    loadCmd = "load"
    pathPart = dropWhile isSpace . drop (length loadCmd)
    handle line = case strip line of
        "" -> again
        "exit" -> end
        "quit" -> end
        str -> addHistory line >> if loadCmd `isPrefixOf` str
            then loadFile (pathPart str) env >>= repl
            else case parse str of
                Left msg -> putErrLn msg >> again
                Right (Assignment mx expr) -> do
                    let evaluated = eval env expr
                    print evaluated
                    repl $ assign mx evaluated env

-- | Reads the indicated file and loads its lines using 'load'. Fails with an
-- error message if the file doesn't exist or if the path string is empty.
loadFile :: String -> Environment -> IO Environment
loadFile "" env = putErrLn "missing file path" >> return env
loadFile path env = do
    exists <- doesFileExist path
    if exists
        then readFile path >>= flip load env . lines
        else putErrLn (path ++ ": file does not exist") >> return env

-- | Evaluates each string as a line of input without printing the results.
-- Returns the augmented environment. It is assumed that the input consists of
-- assignments, since nothing will be done with lone expressions).
load :: [String] -> Environment -> IO Environment
load input env = maybeT failure return augmented
  where
    failure = putErrLn "load failed" >> return env
    nonBlanks = filter (not . null) . map strip $ input
    augmented = foldM accumLine env nonBlanks

-- | Parses and evaluates one line of input, returning the new environment. The
-- 'IO' monad is necessary for printing error messages, and the 'MaybeT' monad
-- transformer is used to terminate the accumulation early as soon as the first
-- error is detected.
accumLine :: Environment -> String -> MaybeT IO Environment
accumLine env str = case parse str of
    Left err -> MaybeT $ putErrLn (str ++ '\n':err) >> return Nothing
    Right (Assignment mx expr) -> return $ assign mx (eval env expr) env

-- | Binds an already evaluated expression to the percentage variable (previous
-- result) and optionally to another token. Returns the updated environment.
assign :: Maybe Token -> Expression -> Environment -> Environment
assign (Just x) expr env = bind x expr $ assign Nothing expr env
assign Nothing expr env = bind (LongVar "%") expr env

-- | Removes leading and trailing whitespace from the string. Also removes a
-- comment beginning with a semicolon, if there is one.
strip :: String -> String
strip = dropWhileEnd isSpace . takeWhile (/= ';') . dropWhile isSpace

-- | Prints a string to standard error.
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr
