module AbstractSyntax where

import Data.List (union, intersect, (\\), nub)

type Var = String
type Output = [Bool]

data Stmt =
    Print Exp Stmt
  | Assign Var Exp Stmt
  | End
  deriving (Eq, Show)

data Exp =
    Variable Var
  | Value Bool
  | And Exp Exp
  | Or Exp Exp
  | Not Exp
  deriving (Eq, Show)

data Type =
    Bool
  | Void
  deriving (Eq, Show)

-- Useful helper functions.
lookup' :: Eq a => a -> [(a, b)] -> b
lookup' x ((x',i) : rest) = if x == x' then i else lookup' x rest

-- Type class for a polymorphic fold function on abstract syntax trees.
--
--  * The first argument is the aggregator for combining
--    results of recursive folds.
--  * The second argument is a function that will be applied to
--    (i.e., and will replace) Variable nodes.
--  * The third argument is a constant that will replace all
--    non-Variable leaf nodes (i.e., Value nodes).
--  * The fourth argument is the abstract syntax tree that
--    will be folded.

class Foldable b where
  fold :: ([a] -> a) -> (String -> a) -> a -> b -> a

instance Foldable Exp where
  fold f var b (Variable x ) = var x
  fold f var b (Value v    ) = b
  fold f var b (And   e1 e2) = f [fold f var b e1, fold f var b e2]
  fold f var b (Or    e1 e2) = f [fold f var b e1, fold f var b e2]
  fold f var b (Not   e    ) = f [fold f var b e]

instance Foldable Stmt where
  fold f var b (Print    e s) = f [fold f var b e, fold f var b s]
  fold f var b (Assign x e s) = f [var x, fold f var b e, fold f var b s]
  --fold f var b _            = ???
  fold f var b (End)          = b

  
combineList :: [[a]] -> [a]
combineList l = foldr (++) [] l

stringToList :: String -> [String]
stringToList str = [str]

class HasVariables a where
  vars :: a -> [Var]

instance HasVariables Stmt where
  vars t = nub (fold combineList stringToList [] t)
  --vars _ = [] -- Implement for Problem #2, part (a).

instance HasVariables Exp where
  vars t = nub (fold combineList stringToList [] t)
  --vars _ = [] -- Implement for Problem #2, part (a).



unbound :: Stmt -> [Var]
unbound (End) = []
unbound (Print e s) = union (vars e) (unbound s)
unbound (Assign x e s) = union ([j | j <- vars s, j /= x]) (vars e)
--unbound _ = [] -- Implement for Problem #2, part (b).



type Interference = [(Var, Var)]

interference :: Stmt -> Interference
interference (End) = []
interference (Print e s) = interference s
interference (Assign x e s) = union (interference s) [(x, y) | y <- (vars e ++ unbound s), y /= x]
--interference _ = [] -- Implement for Problem #2, part (c).


-- eof