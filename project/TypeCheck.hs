module TypeCheck where

import AbstractSyntax
import Parse

class Typeable a where
  chk :: [(String, Type)] -> a -> Maybe Type
  
instance Typeable Exp where 
	chk env (Value v) = Just Bool
	chk env (And e1 e2) =
		let Just tEx1 = chk env e1
		in let Just tEx2 = chk env e2
		in if tEx1 == Bool && tEx2 == Bool then Just Bool else Nothing
	chk env (Or e1 e2) =
		let Just tEx1 = chk env e1
		in let Just tEx2 = chk env e2
		in if tEx1 == Bool && tEx2 == Bool then Just Bool else Nothing
	chk env (Not e) = 
		let Just tEx = chk env e
		in if tEx == Bool then Just Bool else Nothing
	chk env (Variable x) = 
		let [v] = [b | (s, b) <- env, s == x]
		in Just v
	chk _ _ = Nothing -- Implement for Problem #1, part (c).

instance Typeable Stmt where 
	chk env (Print e s) = 
		let Just tSt = chk env s
		in let Just tEx = chk env e
		in if tSt == Void && tEx == Bool then Just Void else Nothing
	chk env (Assign x e s) = 
		let Just tEx = chk env e
		in let Just tSt = chk (env ++ [(x, tEx)]) s
		in if tSt == Void then Just Void else Nothing
	chk env (End) = Just Void
	chk _ _ = Nothing -- Implement for Problem #1, part (c).

-- eof