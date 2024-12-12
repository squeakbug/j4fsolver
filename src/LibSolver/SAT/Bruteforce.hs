module LibSolver.SAT.Bruteforce where

import LibSolver.BoolExpr
  ( BoolExpr(..)
  , freeVariable
  , guessVariable
  , simplify
  , Boolean (bFalse, bTrue)
  , (\/)
  )

unConst :: (Boolean a) => BoolExpr f a -> a
unConst (Const b) = b
unConst _ = error "Not Const"

satisfiable :: (Boolean a) => BoolExpr f a -> a
satisfiable expr =
  case freeVariable expr of
    Nothing -> unConst expr
    Just v ->
      let trueGuess  = simplify (guessVariable v bTrue expr)
          falseGuess = simplify (guessVariable v bFalse expr)
      in satisfiable trueGuess \/ satisfiable falseGuess
