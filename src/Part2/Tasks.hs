module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет
(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
infixl 1 |+|

(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
infixl 1 |-|

(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times
infixl 2 |*|

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
-- переменная - заменяем
replaceVar varName replacement (Variable currVarName) =
  if varName == currVarName then replacement
  else Variable currVarName
-- выражение - проходим по правой и левой части
replaceVar varName replacement (BinaryTerm binOp left right) =
  BinaryTerm binOp correctLeft correctRight where
    correctLeft = replaceVar varName replacement left
    correctRight = replaceVar varName replacement right
-- во всех остальных случаях ничего не делаем
replaceVar _ _ expression = expression


-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
-- считаем +, - и * для двух констант
evaluate (BinaryTerm Plus (IntConstant left) (IntConstant right)) = IntConstant (left + right)
evaluate (BinaryTerm Minus (IntConstant left) (IntConstant right)) = IntConstant (left - right)
evaluate (BinaryTerm Times (IntConstant left) (IntConstant right)) = IntConstant (left * right)
-- для выражений, в которых с одной из сторон при eval'е ничего не меняется, возвращаем само выражение
-- (ничего не меняется -> там есть Variable -> считать ничего не надо)
evaluate (BinaryTerm binOp left right) = 
  if (evalLeft == left) || (evalRight == right) then BinaryTerm binOp left right
  else evaluate (BinaryTerm binOp evalLeft evalRight) where
    evalLeft = evaluate left
    evalRight = evaluate right 

-- в остальных случаях ничего не делаем
evaluate expression = expression

