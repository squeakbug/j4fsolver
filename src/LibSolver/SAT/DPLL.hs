module LibSolver.SAT.DPLL where

-- The only important thing that changed is the
-- addition of the 'where' clause.
satisfiable :: Expr -> Bool
satisfiable expr =
  case freeVariable expr' of
    Nothing -> unConst $ simplify expr'
    Just v ->
      let trueGuess  = simplify (guessVariable v True expr)
          falseGuess = simplify (guessVariable v False expr)
      in satisfiableDPLL trueGuess || satisfiableDPLL falseGuess

  where
    -- Apply our backtracking search *after* literal elimination
    -- and unit propagation have been applied!
    expr' = literalElimination $ cnf $ unitPropagation expr
