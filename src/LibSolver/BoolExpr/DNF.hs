{-# LANGUAGE DataKinds #-}

module LibSolver.BoolExpr.DNF
  ( dnf
  , disjuncts
  , unitPropagation
  , isTautology
  ) where

import Data.Text (Text)
import Data.Maybe (mapMaybe)

import LibSolver.BoolExpr
    ( BoolExpr(And, Or, Var, Not, Const)
    , Boolean
    , BoolExprForm(DNF)
    , guessVariable
    , fixNegations
    , fixDistribute
    , unitClause, complementary
    )
import LibSolver.Util

-- Приведение к ДНФ без проверок
dnfUnsafe :: (Boolean a) => BoolExpr f a -> BoolExpr DNF a
dnfUnsafe (Var x) = Var x
dnfUnsafe (Not x) = Not (dnfUnsafe x)
dnfUnsafe (And x y) = And (dnfUnsafe x) (dnfUnsafe y)
dnfUnsafe (Or x y) = Or (dnfUnsafe x) (dnfUnsafe y)
dnfUnsafe (Const x) = Const x
dnfUnsafe _ = error "Unexpected operation in boolean expression"

-- Приведение к ДНФ
dnf :: (Boolean a) => BoolExpr f a -> BoolExpr DNF a
dnf = dnfUnsafe . fixDistribute . fixNegations

-- Префиксный обход графа:
-- TODO: instance of Traversable trait for BoolExpr
-- TODO: write routine to rebalance boolean tree expression
--       this can improve performance in huge expressions
disjuncts :: Boolean a => BoolExpr DNF a -> [BoolExpr DNF a]
disjuncts (Or x y) = disjuncts x ++ disjuncts y
disjuncts expr = [expr]

-- Выделить все выражения, состоящие из одной переменной
allUnitdisjuncts :: (Boolean a) => BoolExpr DNF a -> [(Text, a)]
allUnitdisjuncts = mapMaybe unitClause . disjuncts

-- Удалить дизъюнкты, содержащие один литерал
unitPropagation :: (Boolean a) => BoolExpr DNF a -> BoolExpr DNF a
unitPropagation expr = replaceAll expr
  where
    assignments = allUnitdisjuncts expr

    replaceAll = foldl (.) id (map (uncurry guessVariable) assignments)

-- | Return 'True' if a disjunction of literals can be demonstrated to be a
--   tautology without need of evaluation - this is the case if it contains two
--   complementary literals.
isTautology :: (Boolean a) => BoolExpr DNF a -> Bool
isTautology expr = any (uncurry complementary) clausePairs
    where
        clausePairs = unorderedPairs (disjuncts expr)
