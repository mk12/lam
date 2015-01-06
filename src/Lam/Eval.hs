-- Copyright 2015 Mitchell Kember. Subject to the MIT License.

-- | Defines 'Expression' and other types to model the lambda-calculus, and
-- implements the 'eval' function for evaluating expressions.
module Lam.Eval
(
-- * Types
  Token(..)
, Expression(..)
, Assignment(..)
, Environment
-- * Evaluation
, eval
-- * Environment
, bind
) where

import Control.Applicative ((<$>))
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import qualified Data.Map as M

-- | A character or sequence of characters that constitute a symbol.
data Token = Var Char       -- ^ A single lowercase letter.
           | LongVar String -- ^ A string not beginning with a lowercase letter.
           deriving (Eq, Ord)

-- | An object in the lambda-calculus.
data Expression
    -- | Evaluates to another expression that is represents within an
    -- environment, or evaluates to itself (a terminal symbol).
    = Symbol Token
    -- | Represents a lambda abstraction with a token as a formal parameter and
    -- any expression as the function body.
    | Function Token Expression
    -- | Evaluates to the application of the first expression to the second,
    -- where the first is the function and the second is the argument.
    | Application Expression Expression
    deriving Eq

-- | Represents the binding of an optionally explicit variable to an expression.
data Assignment = Assignment (Maybe Token) Expression

-- | A mapping from free variables to the expressions they represent.
type Environment = M.Map Token Expression

-- The symbol for the special form If.
specialIf :: Expression
specialIf = Symbol $ LongVar "If"

-- | Evaluates the expression in the given environment by recursively performing
-- symbol lookups as well as alpha, beta, and eta reductions.
eval :: Environment -> Expression -> Expression
eval env s@(Symbol x) = fromMaybe s $ eval (M.delete x env) <$> lookupVal x env
eval env f@(Function _ _) = etaReduce . Function x $ eval (M.delete x env) body
    where (Function x body) = alphaReduce env f
eval env (Application (Application (Application f c) a) b)
    | f == specialIf = case isTrue cond of
        Just True -> eval env a
        Just False -> eval env b
        Nothing -> Application (Application (Application specialIf cond) a) b
    where cond = eval env c
eval env (Application f a) = betaReduce env (eval env f) (eval env a)

-- | Like 'eval', but doesn't perform beta reduction.
reduce :: Environment -> Expression -> Expression
reduce env s@(Symbol x) = fromMaybe s $ reduce (M.delete x env) <$> lookupVal x env
reduce env f@(Function _ _) = Function x $ reduce (M.delete x env) body
    where (Function x body) = alphaReduce env f
reduce env (Application (Application (Application f c) a) b)
    | f == specialIf = case isTrue cond of
        Just True -> reduce env a
        Just False -> reduce env b
        Nothing -> Application (Application (Application specialIf cond) a) b
    where cond = reduce env c
reduce env (Application f a) = Application (reduce env f) (reduce env a)

-- Given a variable and two expressions, substitues the first expression for all
-- occurrences of the variable in the second expression. The substituion is also
-- made inside the alternatives of the If special form.
subst :: Token -> Expression -> Expression -> Expression
subst x e s@(Symbol y)
    | x == y = e
    | otherwise = s
subst x e f@(Function y _)
    | x == y = f
    | otherwise = Function z $ subst x e body
    where (Function z body) = alphaReduce (M.fromList [(x,e)]) f
subst x e (Application f a) = Application (subst x e f) (subst x e a)

-- | Performs alpha-reduction on a function to avoid unintentionally capturing
-- free variables. It does this by renaming the formal parameter and all its
-- occurences in the body so that it is different from all the free variables in
-- the body and different from the symbols they evaluate to.
alphaReduce :: Environment -> Expression -> Expression
alphaReduce env f@(Function x body)
    | S.member x freeFree = Function y $ replace x y body
    | otherwise = f
  where
    free = freeVariables f
    freeFree = unionMap (freeVariables . reduce env . Symbol) free
    y = genToken $ S.unions [freeFree, allVariables f]
alphaReduce _ expr = expr

-- | Applies one expression to another if the first expression is a function.
-- Otherwise, returns an application object.
betaReduce :: Environment -> Expression -> Expression -> Expression
betaReduce env f@(Function x body) a
    | S.member x (freeVariables a) = eval env $ subst y a (replace x y body)
    | otherwise = eval env $ subst x a body
    where y = genToken $ S.union (allVariables f) (freeVariables a)
betaReduce _ f a = Application f a

-- | Performs eta-reduction on the expression (note that the environment is not
-- required for this type of reduction).
etaReduce :: Expression -> Expression
etaReduce (Function x (Application f (Symbol y)))
    | x == y && S.notMember x (freeVariables f) = f
etaReduce expr = expr

-- | Returns the Boolean value represented by the expression, if possible.
isTrue :: Expression -> Maybe Bool
isTrue (Function x (Function y (Symbol z)))
    | x /= y && x == z = Just True
    | x /= y && y == z = Just False
isTrue _ = Nothing

-- | Like 'concatMap', but for sets. Applies the function to each element in the
-- set and returns the union of all these sets.
unionMap :: Ord b => (a -> S.Set b) -> S.Set a -> S.Set b
unionMap f = S.unions . S.toList . S.map f

-- | Generates a new token different from all the tokens in the set.
genToken :: S.Set Token -> Token
genToken ts = head . dropWhile (`S.member` ts) . map makeVar $ [(0::Int)..]
    where makeVar = LongVar . ('_' :) . show

-- | Replaces all occurences of free variables represented by the first token to
-- symbols of the second token in an expression, returning the new expression.
replace :: Token -> Token -> Expression -> Expression
replace x y s@(Symbol z)
    | z == x = Symbol y
    | otherwise = s
replace x y f@(Function z body)
    | x == z = f
    | y == z = error "replacement would bind variables"
    | otherwise = Function z $ replace x y body
replace x y (Application f a) = Application (replace x y f) (replace x y a)

-- | Returns the set of all variables (bound and free) in an expression.
-- Equivalent to the union of the sets returned by 'freeVariables' and
-- 'boundVariables', but more efficient.
allVariables :: Expression -> S.Set Token
allVariables (Symbol x) = S.singleton x
allVariables (Function x body) = S.insert x $ allVariables body
allVariables (Application f a) = S.union (allVariables f) (allVariables a)

-- | Returns the set of all the free variables in the expression.
freeVariables :: Expression -> S.Set Token
freeVariables (Symbol x) = S.singleton x
freeVariables (Function x body) = S.delete x $ freeVariables body
freeVariables (Application f a) = S.union (freeVariables f) (freeVariables a)

-- | Returns the set of all the bound variables in the expression.
-- boundVariables :: Expression -> S.Set Token
-- boundVariables (Symbol _) = S.empty
-- boundVariables (Function x body) = S.insert x $ boundVariables body
-- boundVariables (Application f a) = S.union (boundVariables f) (boundVariables a)

-- | Looks up the value of a token in an environment. If the token is a string
-- of digits, returns a Church numeral expression instead.
lookupVal :: Token -> Environment -> Maybe Expression
lookupVal (LongVar str) _
    | all isDigit str = Just $ churchNumeral (read str :: Int)
lookupVal x env = M.lookup x env

-- | Constructs a Church numeral for the given natural number.
churchNumeral :: Int -> Expression
churchNumeral n = Function f . Function x $ inside n
  where
    f = Var 'f'
    x = Var 'x'
    inside 0 = Symbol x
    inside m = Application (Symbol f) $ inside (pred m)

-- | Binds a variable to an expression in an environment, returning the updated
-- environment containing the new binding. Returns the unchanged environment if
-- the binding would create a direct self reference.
bind :: Token -> Expression -> Environment -> Environment
bind x (Symbol y) env | x == y = env
bind x expr env = M.insert x expr env

instance Show Token where
    show (Var c) = [c]
    show (LongVar s) = s

instance Show Expression where
    show (Symbol x) = show x
    show (Function x body) = "\\" ++ show x ++ showRest body
    show (Application f a@(Application _ _)) = show f ++ " " ++ parens (show a)
    show (Application f a)
        | functionRight f = parens (show f) ++ " " ++ show a
        | otherwise = show f ++ " " ++ show a

-- | Helper function for showing lambda functions.
showRest :: Expression -> String
showRest (Function x body) = " " ++ show x ++ showRest body
showRest expr = "." ++ show expr

-- | Returns true if the rightmost part of the expression is a function.
functionRight :: Expression -> Bool
functionRight (Application _ a) = functionRight a
functionRight (Function _ _) = True
functionRight _ = False

-- | Wraps parentheses around a string.
parens :: String -> String
parens str = "(" ++ str ++ ")"
