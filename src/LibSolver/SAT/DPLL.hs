{-# LANGUAGE DataKinds #-}

module LibSolver.SAT.DPLL where

import LibSolver.BoolExpr
  ( BoolExpr(..)
  , BoolExprForm(CNF)
  , simplify
  , freeVariable
  , guessVariable
  , literalElimination
  , Boolean (bTrue, bFalse, (\/))
  )
import LibSolver.BoolExpr.CNF (cnf, unitPropagation)

unConst :: (Boolean a) => BoolExpr f a -> a
unConst (Const b) = b
unConst _ = error "Not Const"

-- The only important thing that changed is the
-- addition of the 'where' clause.
satisfiable :: (Boolean a) => BoolExpr CNF a -> a
satisfiable expr =
  case freeVariable expr' of
    Nothing -> unConst $ simplify expr'
    Just v ->
      let trueGuess  = simplify (guessVariable v bTrue expr)
          falseGuess = simplify (guessVariable v bFalse expr)
      in satisfiable trueGuess \/ satisfiable falseGuess

  where
    -- Apply our backtracking search *after* literal elimination
    -- and unit propagation have been applied!
    expr' = literalElimination $ cnf $ unitPropagation expr
