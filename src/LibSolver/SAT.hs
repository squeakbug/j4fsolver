module LibSolver.SAT where

import LibSolver.BoolExpr (Boolean, BoolExpr)

class (Boolean a) => SATSolver a where
    satisfiable :: BoolExpr f a -> Bool
