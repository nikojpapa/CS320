module Compile where

import AbstractSyntax
import Allocation
import Machine
import TypeCheck

class Compilable a where
  comp :: [(Var, Register)] -> a -> Instruction

instance Compilable Stmt where
  comp xrs (End) = STOP (Register 0)
  comp xrs (Print e s) = 
    let eInst = comp xrs e
    in let sInst = comp xrs s
    in eInst +++ (COPY (Register 0) (register eInst) (STOP (Register 0))) +++ sInst
  comp xrs (Assign x e s) = 
    let eInst = comp xrs e
    in let sInst = comp xrs s
    in let xReg = lookup' x xrs
    in eInst +++ (COPY xReg (register eInst) (STOP xReg)) +++ sInst
  --comp _    _    = STOP (Register -1) -- Complete missing cases for Problem #4, part (b).

instance Compilable Exp where
  comp xrs (Variable x) = STOP (lookup' x xrs)
  comp xrs (Value False) = let newReg = (maximum [r | (x, r) <- xrs] + 1) in INIT (newReg) (STOP newReg)
  comp xrs (Value True) = let newReg = (maximum [r | (x, r) <- xrs] + 1) in INIT (newReg) (FLIP (newReg) (STOP newReg))
  comp xrs (And e1 e2) =
    let eInst1 = comp xrs e1
    in let eReg1 = register eInst1
    in let eInst2 = comp (("e1", eReg1):xrs) e2
    in let eReg2 = register eInst2
    in let outReg = (maximum [r | (x, r) <- ([("e1", eReg1),("e2", eReg2)] ++ xrs)] + 1)
    in eInst1 +++ eInst2 +++ (NAND eReg1 eReg2 outReg (FLIP outReg (STOP outReg)))
  comp xrs (Or e1 e2) =
    let eInst1 = comp xrs e1
    in let eReg1 = register eInst1
    in let eInst2 = comp (("e1", eReg1):xrs) e2
    in let eReg2 = register eInst2
    in let outReg = (maximum [r | (x, r) <- ([("e1", eReg1),("e2", eReg2)] ++ xrs)] + 1)
    in eInst1 +++ eInst2 +++ (FLIP eReg1 (FLIP eReg2 (NAND eReg1 eReg2 outReg (STOP outReg))))
  comp xrs (Not e) = 
    let eInst = comp xrs e
    in let eReg = register eInst
    in eInst +++ (FLIP eReg (STOP eReg))
  --comp _   _ = STOP (Register -1) -- Complete missing cases for Problem #4, part (b).

compileMin :: Stmt -> Maybe Instruction
compileMin s = 
  if chk [] s == Just Void then 
    let Alloc rs = smallest (allocations (interference s, [Register (toInteger r) | r <- [1..length (vars s)]]) (Alloc []) (vars s))
    in Just (comp rs s)
  else Nothing
--compileMin _ = STOP (Register -1) -- Complete for Problem #4, part (c).

compileMax :: Integer -> Stmt -> Maybe Instruction
compileMax k s = 
  if chk [] s == Just Void then 
    if length (interference s) < (fromIntegral k) then
      let Alloc rs = largest (allocations (interference s, [Register (toInteger r) | r <- [1..k]]) (Alloc []) (vars s))
      in Just (comp rs s)
    else Nothing
  else Nothing
--compileMax _ _ = Nothing -- Complete for Problem #4, part (d).

-- eof