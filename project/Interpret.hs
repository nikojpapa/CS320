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
	let v = lookup' x env
	in v
--eval _ _ = False -- Implement for Problem #1, part (b).

exec :: [(String, Bool)] -> Stmt -> ([(String, Bool)], Output)
exec env (Print    e s) =
  let (env', o) = exec env s
  in (env', [eval env e] ++ o)
exec env (Assign x e s) = 
	exec (assignValue x (eval env e) env) s
exec env (End) = (env, [])
--exec env _ = (env, []) -- Implement the Assign and End cases for Problem #1, part (b).

assignValue :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
assignValue key value assoc = (key,value):(filter ((key /=).fst) assoc)

interpret :: Stmt -> Maybe Output
interpret s = 
	if chk [] s == Just Void then 
		let (env, o) = exec [] s
		in Just o
	else Nothing
--interpret _ = Nothing -- Implement for Problem #1, part (d). 

-- 4d) Interpret will be passed a statement, as defined by the function declaration.  This statement will have to pass through chk, which will make sure it is a valid statment.  If there were unbound variables, chk would return nothing since it would not be able to find a type of that variable.

-- eof