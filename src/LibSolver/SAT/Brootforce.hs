module LibSolver.SAT.Brootforce where

unConst :: Expr -> Bool
unConst (Const b) = b
unConst _ = error "Not Const"

satisfiable :: Expr -> Bool
satisfiable expr =
  case freeVariable expr of
    Nothing -> unConst expr
    Just v ->
      let trueGuess  = simplify (guessVariable v True expr)
          falseGuess = simplify (guessVariable v False expr)
      in satisfiable trueGuess || satisfiable falseGuess
