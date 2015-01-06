-- Copyright 2015 Mitchell Kember. Subject to the MIT License.

-- | Uses "Text.Parsec" to parse lambda expressions.
module Lam.Parse (parse, whiteChars) where

import Control.Applicative ((<$>), (<*>), (*>), (<*))
import Data.List (delete)
import Text.Parsec.Error (errorMessages, showErrorMessages)
import Text.Parsec.Prim ((<|>), (<?>))
import Text.Parsec.String (Parser)
import qualified Text.Parsec as P

import Lam.Eval (Assignment(..), Expression(..), Token(..))

-- | Parses a string, returning an assignment or an error string.
parse :: String -> Either String Assignment
parse = newEither showErr id . P.parse (assignment <* P.eof) ""

-- | Parses an assignment, which is an explicit binding or a lone expression.
assignment :: Parser Assignment
assignment = Assignment
    <$> P.optionMaybe (P.try (strip token <* P.char '=')) <*> expression

-- | Parses a single lambda-calculus expression.
expression :: Parser Expression
expression = whitespace *> (function <|> application)

-- | Parses a lambda function of one parameter, or a curried function from the
-- abbreviated form where multiple parameters appear before the period.
function :: Parser Expression
function = P.char '\\' *> rest <?> "function"
  where
    rest = Function <$> strip token <*> (rest <|> P.char '.' *> expression)

-- | Parses one or more expressions as a symbol or application.
application :: Parser Expression
application = foldl1 Application <$> many1Last term function

-- | Parses a symbol or a parenthesized expression.
term :: Parser Expression
term = strip $ Symbol <$> tokenD <|> pExpr
  where
    pExpr = P.between (P.char '(') (P.char ')') expression <?> "application"

-- | Parses a single token.
token :: Parser Token
token = token' False

-- | Parses a single token, allowing it to begin with a digit.
tokenD :: Parser Token
tokenD = token' True

-- | Helper function used for 'token' and 'tokenD'.
token' :: Bool -> Parser Token
token' d = Var <$> P.lower
    <|> LongVar <$>
        ((:) <$> P.noneOf excluded
             <*> P.many (P.noneOf reserved))
    <?> "variable"
    where excluded = reserved ++ ['a'..'z'] ++ if d then [] else ['0'..'9']

-- | Strips whitespace on either side of a parser.
strip :: Parser a -> Parser a
strip p = whitespace *> p <* whitespace

-- | Skips zero or more whitespace characters.
whitespace :: Parser ()
whitespace = P.skipMany $ P.oneOf whiteChars

-- | A string of characters that count as whitespace.
whiteChars :: String
whiteChars = " \t"

-- | A list of reserved characters that may not be used in tokens.
reserved :: String
reserved = "()\\.= \t"

-- | Applies the first parser one or more times, and after this applies the
-- second parser zero times or one time. Returns a list of the returned values.
many1Last :: Parser a -> Parser a -> Parser [a]
many1Last p q = do
    x <- p
    xs <- P.many p
    my <- P.optionMaybe q
    return $ case my of
        Nothing -> x:xs
        Just y -> x:xs ++ [y]

-- | Shows a parser error as a user-readable string.
showErr :: P.ParseError -> String
showErr = delete '\\' -- Because Parsec says "\\" instead of "\".
        . tail
        . showErrorMessages
        "or" "unknown error" "expecting" "unexpected" "end of input"
        . errorMessages

-- | Transforms both sides of an either structure by different functions.
newEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
newEither f _ (Left x) = Left $ f x
newEither _ f (Right x) = Right $ f x
