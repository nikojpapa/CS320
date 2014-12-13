module Compile where

import AbstractSyntax
import Allocation
import Machine

class Compilable a where
  comp :: [(Var, Register)] -> a -> Instruction

instance Compilable Stmt where
  comp xrs (End) = STOP (Register 0)
  comp xrs (Print e s) = COPY (Register 0) (register (comp e)) (comp s)
  comp xrs (Assign x e s) = COPY (lookup' x xrs) (register (comp e)) (comp s)
  comp _    _    = STOP (Register -1) -- Complete missing cases for Problem #4, part (b).

instance Compilable Exp where
  comp xrs (Variable x) = STOP (lookup' x xrs)
  comp xrs (Value False) = 
  comp _   _ = STOP (Register -1) -- Complete missing cases for Problem #4, part (b).

compileMin :: Stmt -> Maybe Instruction
compileMin _ = STOP (Register -1) -- Complete for Problem #4, part (c).

compileMax :: Integer -> Stmt -> Maybe Instruction
compileMax _ _ = Nothing -- Complete for Problem #4, part (d).

-- eof