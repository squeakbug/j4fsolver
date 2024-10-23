module LibSolver.SAT where

import LibSolver.BoolExpr (Expr)

class SATSolver where
    satisfiable :: Expr -> Bool
