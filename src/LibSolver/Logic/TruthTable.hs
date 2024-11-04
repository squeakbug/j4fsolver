module LibSolver.Logic.TruthTable where

-- |Конкретный экземпляр базы знаний пропозициональной логики, 
--  использующий таблицы истинности для выводов.
data TruthTableKB p t = TT [PLExpr]

-- |The 'KB' instance for a knowledge base that uses truth tables for inference.
instance KB TruthTableKB PLExpr Bool where
    empty              = TT []
    tell     (TT ps) p = TT $ ps ++ conjuncts (toCnf p)
    retract  (TT ps) p = TT $ L.delete p ps
    ask      (TT ps) p = ttEntails (And ps) p
    askVars            = undefined
    axioms   (TT ps)   = ps