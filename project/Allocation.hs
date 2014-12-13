module Allocation where

import Data.List (union, intersect, (\\), nub)
import AbstractSyntax
import Machine

data Tree a =
    Branch a [Tree a]
  | Finish a
  deriving (Eq, Show)

foldTree :: ([a] -> a) -> Tree a -> a
foldTree f (Finish d) = d
foldTree f (Branch d ts) = f ([d] ++ [foldTree f t | t <- ts])

listOfLeaves :: Tree a -> [a]
listOfLeaves (Finish d) = [d]
listOfLeaves (Branch d ts) = concat [listOfLeaves t | t <- ts]

smallest :: Ord a => Tree a -> a
smallest t = minimum (listOfLeaves t)

largest :: Ord a => Tree a -> a
largest t = maximum (listOfLeaves t) -- Complete for Problem #3, part (b).

closeToK :: Int -> Tree Allocation -> Allocation
closeToK k t = maximum [l | l <- listOfLeaves t, distinctRegs l <= k]

data Allocation =
    Alloc [(Var, Register)]
  deriving (Eq, Show)

distinctRegs :: Allocation -> Int
distinctRegs (Alloc l) = length (nub [r | (v, r) <- l])

instance Ord Allocation where
	compare a a' = 
		if distinctRegs a == distinctRegs a' then EQ
		else if distinctRegs a <= distinctRegs a' then LT
		else GT
-- Add instance declaration for Problem #3, part (c) here.


allocations :: (Interference, [Register]) -> Allocation -> [Var] -> Tree Allocation
allocations (conflicts, rs) (Alloc a) (x:xs) = 
	let choices = unconflicted (conflicts, rs) (Alloc a) x
	in if length xs > 0 then Branch (Alloc a) [allocations (conflicts, rs) (Alloc (a ++ [(x, regs)])) xs | regs <- choices]
		else Branch (Alloc a) [Finish (Alloc (a ++ [(x, regs)])) | regs <- choices]

-- Useful helper function.
unconflicted ::(Interference, [Register]) -> Allocation -> Var -> [Register]
unconflicted (conflicts, rs) (Alloc a) x = rs \\ [r | (y,r) <- a, (x,y) `elem` conflicts || (y,x) `elem` conflicts]

--eof