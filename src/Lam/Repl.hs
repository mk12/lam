-- Copyright 2015 Mitchell Kember. Subject to the MIT License.

-- | Implements the read-eval-print loop in the 'repl' function.
module Lam.Repl (loadFile, startRepl) where

import Control.Error.Util (maybeT)
import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.List.Split (splitWhen)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import qualified System.Console.Haskeline as H

import Lam.Eval
import Lam.Parse

-- | Sets up "Haskeline" and starts the read-eval-print loop.
startRepl :: Environment -> IO ()
startRepl env = H.runInputT settings (repl env)
    where settings = H.setComplete completion H.defaultSettings

-- | Performs the read-eval-print loop forever: prompts the user, reads input,
-- parses it, evaluates it, prints the result, and repeats. The environment is
-- updated on each iteration with a new binding if the user enters an assignment
-- (and if they only enter an expression, it will still be bound to the percent
-- variable). Also handles special commands for loading files and exiting.
repl :: Environment -> H.InputT IO ()
repl env = maybe end handle =<< H.getInputLine "lam> "
  where
    end = return ()
    again = repl env
    nullOrSpace = maybe True isSpace . listToMaybe
    handle line = case stripC line of
        "" -> again
        "exit" -> end
        "quit" -> end
        'l':'o':'a':'d':str | nullOrSpace str ->
            let env' = loadFile (strip str) env
            in repl =<< liftIO env'
        str -> case parse str of
            Left msg -> liftIO (putErrLn msg) >> again
            Right (Assignment mx expr) -> do
                let evaluated = process $ eval env expr
                H.outputStrLn $ show evaluated
                repl $ assign mx evaluated env

-- | Reads the indicated file and loads its lines using 'load'. Fails with an
-- error message if the file doesn't exist or if the path string is empty.
loadFile :: String -> Environment -> IO Environment
loadFile "" env = putErrLn "missing file path" >> return env
loadFile path env = doesFileExist path' >>= go
  where
    path' = filter (not . flip elem "\\'\"") . strip $ path
    failure msg = putErrLn (path' ++ ": " ++ msg) >> return env
    env' = liftIO (readFile path') >>= flip load env . splitLines
    go True = maybeT (failure "load failed") return env'
    go False = failure "file does not exist"

-- | Like `lines`, but combines multiple lines into one if the lines following
-- the first one are indented. Removes newlines in both cases.
splitLines :: String -> [String]
splitLines "" = []
splitLines str = map unpair . splitWhen eos . zip str $ offset
  where
    offset = tail str ++ "\n"
    eos (x, next) = x == '\n' && notElem next whiteChars
    unpair = filter (/= '\n') . map fst

-- | Processes an already evaluated expression. This is used to implement some
-- special evaluation rules that are impossible to implement in Lam itself.
process :: Expression -> Expression
process app@(Application s a)
    | s == symStr "#" = case a of
        -- Special case for the eta-reduction of 1 to the identity function.
        Function f (Symbol g) | f == g -> symStr "1"
        Function f (Function x body) -> fromMaybe app $ decodeNum f x body
        _ -> app
    | s == symStr "?" = case a of
        Function x (Function y (Symbol z)) -> fromMaybe app $ decodeBool x y z
        _ -> app
process expr = expr

-- | Tries to decode a Church numeral given the parameters and function body.
decodeNum :: Token -> Token -> Expression -> Maybe Expression
decodeNum f x body = symStr . show <$> go body 0
  where
    go (Symbol y) n | y == x = Just n :: Maybe Int
    go (Application (Symbol g) expr) n | g == f = go expr (succ n)
    go _ _ = Nothing

-- | Tries to decode a Church Boolean value given the parameters and the
-- variable in the function body (which is just a symbol).
decodeBool :: Token -> Token -> Token -> Maybe Expression
decodeBool x y z
    | x /= y && x == z = Just $ symStr "True"
    | x /= y && y == z = Just $ symStr "False"
decodeBool _ _ _ = Nothing

-- | Creates a symbol from a string.
symStr :: String -> Expression
symStr = Symbol . LongVar

-- | Evaluates each string as a line of input without printing the results.
-- Returns the augmented environment. It is assumed that the input consists of
-- assignments, since nothing will be done with lone expressions).
load :: [String] -> Environment -> MaybeT IO Environment
load input env = foldM accumLine env nonBlanks
    where nonBlanks = filter (not . null) . map stripC $ input

-- | Parses and evaluates one line of input, returning the new environment. The
-- 'IO' monad is necessary for printing error messages, and the 'MaybeT' monad
-- transformer is used to terminate the accumulation early as soon as the first
-- error is detected.
accumLine :: Environment -> String -> MaybeT IO Environment
accumLine env str = case parse str of
    Left err -> failure str err
    Right (Assignment mx expr) -> case mx of
        Nothing -> failure str "expected assignment"
        Just x -> return $ bind x (eval env expr) env
  where
    failure s = MaybeT . (>> return Nothing) . putErrLn . quote s
    quote s msg = "\"" ++ s ++ "\"\n" ++ msg

-- | Binds an already evaluated expression to the percentage variable (previous
-- result) and optionally to another token. Returns the updated environment.
assign :: Maybe Token -> Expression -> Environment -> Environment
assign (Just x) expr env = bind x expr $ assign Nothing expr env
assign Nothing expr env = bind (LongVar "%") expr env

-- | Removes a comment beginning with a semicolon, if any, and strips whitespace
-- from the string using 'strip'.
stripC :: String -> String
stripC = strip . takeWhile (/= ';')

-- | Removes leading and trailing whitespace from the string.
strip :: String -> String
strip = dropWhileEnd isSpace . dropWhile isSpace

-- | Prints a string to standard error.
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

-- | Completion function which acts like 'H.completeFilename' only when the
-- input line begins with "load". Otherwise, no completion is offered.
completion :: (Functor m, MonadIO m) => H.CompletionFunc m
completion = H.completeWordWithPrev Nothing whiteChars $ \rev word ->
    if strip (reverse rev) == "load"
        then snd <$> H.completeFilename (reverse word, "")
        else return []
