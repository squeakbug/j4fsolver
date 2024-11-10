{-# LANGUAGE DataKinds #-}

module LibSolver.BoolExpr.CNF
  ( cnf
  , conjuncts
  , unitPropagation
  ) where

import Data.Text (Text)
import Data.Maybe (mapMaybe)

import LibSolver.BoolExpr
    ( BoolExpr(And, Or, Var, Not, Const)
    , Boolean
    , BoolExprForm(CNF)
    , guessVariable
    , fixNegations
    , fixDistribute
    , unitClause
    )

-- Приведение к КНФ без проверок
cnfUnsafe :: (Boolean a) => BoolExpr f a -> BoolExpr CNF a
cnfUnsafe (Var x) = Var x
cnfUnsafe (Not x) = Not (cnfUnsafe x)
cnfUnsafe (And x y) = And (cnfUnsafe x) (cnfUnsafe y)
cnfUnsafe (Or x y) = Or (cnfUnsafe x) (cnfUnsafe y)
cnfUnsafe (Const x) = Const x
cnfUnsafe _ = error "Unexpected operation in boolean expression"

-- Приведение к КНФ
cnf :: (Boolean a) => BoolExpr f a -> BoolExpr CNF a
cnf = cnfUnsafe . fixDistribute . fixNegations

-- Префиксный обход графа:
-- TODO: instance of Traversable trait for BoolExpr
-- TODO: write routine to rebalance boolean tree expression
--       this can improve performance in huge expressions
conjuncts :: Boolean a => BoolExpr CNF a -> [BoolExpr CNF a]
conjuncts (And x y) = conjuncts x  ++ conjuncts y
conjuncts expr = [expr]

-- Выделить все выражения, состоящие из одной переменной
allUnitconjuncts :: (Boolean a) => BoolExpr CNF a -> [(Text, a)]
allUnitconjuncts = mapMaybe unitClause . conjuncts

-- Удалить конъюнкты, содержащие один литерал
unitPropagation :: (Boolean a) => BoolExpr CNF a -> BoolExpr CNF a
unitPropagation expr = replaceAll expr
  where
    assignments = allUnitconjuncts expr
    replaceAll = foldl (.) id (map (uncurry guessVariable) assignments)

