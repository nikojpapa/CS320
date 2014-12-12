module Interpret where

import AbstractSyntax
import Parse
import TypeCheck

eval :: [(String, Bool)] -> Exp -> Bool
eval env (Value v) = v
eval env (And e1 e2) = 
	let v1 = eval env e1
	in let v2 = eval env e2
	in v1 && v2
eval env (Or e1 e2) = 
	let v1 = eval env e1
	in let v2 = eval env e2
	in v1 || v2
eval env (Not e) = 
	let v = eval env e
	in not v
eval env (Variable x) = 
	let [v] = [b | (s, b) <- env, s == x]
	in v
--eval _ _ = False -- Implement for Problem #1, part (b).

exec :: [(String, Bool)] -> Stmt -> ([(String, Bool)], Output)
exec env (Print    e s) =
  let (env', o) = exec env s
  in (env', [eval env e] ++ o)
exec env (Assign x e s) = 
	exec (env ++ [(x, eval env e)]) s
exec env (End) = (env, [])
--exec env _ = (env, []) -- Implement the Assign and End cases for Problem #1, part (b).

interpret :: Stmt -> Maybe Output
interpret s = 
	if chk [] s == Just Void then 
		let (env, o) = exec [] s
		in Just o
	else Nothing
interpret _ = Nothing -- Implement for Problem #1, part (d). 

-- eof