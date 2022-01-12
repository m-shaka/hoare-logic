module Lib
  ( someFunc,
  )
where

import Numeric.Natural

-- プログラムの構文
newtype Variable = Variable String deriving (Show, Eq)

data Arithmetic
  = Nat Natural
  | Var Variable
  | Add Arithmetic Arithmetic
  | Multi Arithmetic Arithmetic
  deriving (Show, Eq)

data PBool
  = PTrue
  | PFalse
  | Leq Arithmetic Arithmetic
  | Not PBool
  | And PBool PBool
  deriving (Show, Eq)

data Program
  = Skip
  | Assign Variable Arithmetic
  | Seq Program Program
  | If PBool Program Program
  | While Condition PBool Program
  deriving (Show, Eq)

-- 条件式の構文
data ConditionExp
  = MetaVar String
  | CNat Natural
  | CVar Variable
  | CAdd ConditionExp ConditionExp
  | CMulti ConditionExp ConditionExp
  deriving (Show, Eq)

-- 面倒なのでorとならばを追加した
data Condition
  = CTrue
  | CFalse
  | CLeq ConditionExp ConditionExp
  | CNot Condition
  | CAnd Condition Condition
  | COr Condition Condition
  | CImplication Condition Condition
  | CExists Condition
  deriving (Show, Eq)

arithToExp :: Arithmetic -> ConditionExp
arithToExp (Nat n) = CNat n
arithToExp (Var v) = CVar v
arithToExp (Add e1 e2) = CAdd (arithToExp e1) (arithToExp e2)
arithToExp (Multi e1 e2) = CMulti (arithToExp e1) (arithToExp e2)

boolToCondition :: PBool -> Condition
boolToCondition PTrue = CTrue
boolToCondition PFalse = CFalse
boolToCondition (Leq a1 a2) = CLeq (arithToExp a1) (arithToExp a2)
boolToCondition (Not b) = CNot $ boolToCondition b
boolToCondition (And a1 a2) = CAnd (boolToCondition a1) (boolToCondition a2)

substVarToExp :: ConditionExp -> Variable -> Arithmetic -> ConditionExp
substVarToExp v@(CVar var1) var2 arith =
  if var1 == var2 then arithToExp arith else v
substVarToExp (CAdd e1 e2) var arith = CAdd (substVarToExp e1 var arith) (substVarToExp e2 var arith)
substVarToExp (CMulti e1 e2) var arith = CMulti (substVarToExp e1 var arith) (substVarToExp e2 var arith)
substVarToExp e _ _ = e

subst :: Condition -> Variable -> Arithmetic -> Condition
subst CTrue _ _ = CTrue
subst CFalse _ _ = CFalse
subst (CLeq e1 e2) var arith = CLeq (substVarToExp e1 var arith) (substVarToExp e2 var arith)
subst (CNot e) var arith = CNot $ subst e var arith
subst (CAnd e1 e2) var arith = CAnd (subst e1 var arith) (subst e2 var arith)
subst (COr e1 e2) var arith = COr (subst e1 var arith) (subst e2 var arith)
subst (CImplication e1 e2) var arith = CImplication (subst e1 var arith) (subst e2 var arith)
subst (CExists c) var arith = CExists $ subst c var arith

awp :: Program -> Condition -> (Condition, Condition)
awp Skip b = (b, CTrue)
awp (Assign var arith) b = (subst b var arith, CTrue)
awp (Seq p0 p1) b = (a0, CAnd c0 c1)
  where
    (a1, c1) = awp p1 b
    (a0, c0) = awp p0 a1
awp (If cond t e) b = (COr thenCond elseCond, CAnd c0 c1)
  where
    (a0, c0) = awp t b
    (a1, c1) = awp e b
    thenCond = CAnd a0 (boolToCondition cond)
    elseCond = CAnd a1 (CNot (boolToCondition cond))
awp (While i cond c) b = (i, CAnd loopCond (CAnd breakCond c'))
  where
    (a, c') = awp c i
    loopCond = CImplication (CAnd i b) a
    breakCond = CImplication (CAnd i (CNot b)) b

vp :: Program -> Condition -> Condition -> Condition
vp p pre post = CAnd (CImplication pre pre') c
  where
    (pre', c) = awp p post

someFunc :: IO ()
someFunc = do
  print $ vp Skip CTrue CTrue
  print $ vp (Assign (Variable "X") (Nat 3)) CTrue (CLeq (CVar $ Variable "X") (CNat 2))
