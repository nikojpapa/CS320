module Compile where

import AbstractSyntax
import Allocation
import Machine
import TypeCheck

class Compilable a where
  comp :: [(Var, Register)] -> a -> Instruction

instance Compilable Stmt where
  comp xrs (End) = STOP (Register 0)
  comp xrs (Print e s) = COPY (Register 0) (register (comp xrs e)) (comp xrs s)
  comp xrs (Assign x e s) = COPY (lookup' x xrs) (register (comp xrs e)) (comp xrs s)
  --comp _    _    = STOP (Register -1) -- Complete missing cases for Problem #4, part (b).

instance Compilable Exp where
  comp xrs (Variable x) = STOP (lookup' x xrs)
  comp xrs (Value False) = let newReg = (Register ((maximum [r | (x, Register r) <- xrs]) + 1)) in INIT (newReg) (STOP newReg)
  comp xrs (Value True) = let newReg = (Register ((maximum [r | (x, Register r) <- xrs]) + 1)) in INIT (newReg) (FLIP (newReg) (STOP newReg))
  comp xrs (And e1 e2) = let newReg = (Register ((maximum [r | (x, Register r) <- xrs]) + 1)) + 1 in NAND (register (comp (("newReg1", newReg):xrs) e1)) (register (comp (("newReg2", newReg + 1):xrs) e2)) newReg (FLIP newReg (STOP newReg))
  comp xrs (Not e) = let newReg = (Register ((maximum [r | (x, Register r) <- xrs]) + 1)) in FLIP (register (comp (("newReg1", newReg):xrs) e)) (STOP newReg)
  --comp _   _ = STOP (Register -1) -- Complete missing cases for Problem #4, part (b).

compileMin :: Stmt -> Maybe Instruction
compileMin s = 
  if chk [] s == Just Void then 
    let Alloc rs = smallest (allocations (interference s, [Register (toInteger r) | r <- [0,1..length (unbound s)]]) (Alloc []) (unbound s))
    in Just (comp rs s)
  else Nothing
--compileMin _ = STOP (Register -1) -- Complete for Problem #4, part (c).

compileMax :: Integer -> Stmt -> Maybe Instruction
compileMax k s= 
  if chk [] s == Just Void then 
    let Alloc rs = closeToK (fromInteger k) (allocations (interference s, [Register (toInteger r) | r <- [0,1..length (unbound s)]]) (Alloc []) (unbound s))
    in Just (comp rs s)
  else Nothing
--compileMax _ _ = Nothing -- Complete for Problem #4, part (d).

-- eof