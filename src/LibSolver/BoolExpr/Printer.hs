module LibSolver.BoolExpr.Printer
  ( boolExprPrinter
  )
where

import LibSolver.BoolExpr

-- | Printer
boolExprPrinter :: (a -> ShowS) -> BoolExpr a -> ShowS
boolExprPrinter f = go
  where
    go (Var text) = text
    go (And a b)  = paren $ go a . text " AND " . go b
    go (Or a b) = paren $ go a . text " OR "  . go b
    go (Not a)   = text "-" . paren (go a)
    go (Const c) = text c

sep :: String -> String -> (a -> ShowS) -> [a] -> ShowS
sep empty _ _ [] = text empty
sep _     s f xs = foldr1 (\x y -> x . text s . y) (f <$> xs)

-- disjPrinter :: (a -> ShowS) -> Disj a -> ShowS
-- disjPrinter f = sep "FALSE" " OR " (paren . f) . unDisj

-- conjPrinter :: (a -> ShowS) -> Conj a -> ShowS
-- conjPrinter f = sep "TRUE" " AND " (paren . f) . unConj

-- cnfPrinter :: (a -> ShowS) -> CNF a -> ShowS
-- cnfPrinter f = conjPrinter (disjPrinter (signedPrinter f)) . unCNF

-- dnfPrinter :: (a -> ShowS) -> DNF a -> ShowS
-- dnfPrinter f = disjPrinter (conjPrinter (signedPrinter f)) . unDNF

paren :: ShowS -> ShowS
paren = showParen True

text :: String -> ShowS
text  = showString