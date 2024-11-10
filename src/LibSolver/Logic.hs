module LibSolver.Logic where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

import LibSolver.BoolExpr

-- | База знаний
--   k - тип базы знаний (Таблица, граф, ...)
--   p - тип правила (CNF, DNF, ...)
--   t - внутренний тип (Haskell's Bool, User defined type with boolean ring properties, ...)
class (Eq p, Show t) => KB k p t where
    -- | Пустая база знаний
    empty :: k p t

    -- |Сохранить правило в базе знаний
    tell :: k p t -> p -> k p t

    -- |Проверить, выполнимо ли для данной базы знаний заданное правило
    ask :: k p t -> p -> Bool

    -- |Множество термов правила, "приводящих" к выполнению заданного правила
    askVars :: k p t -> p -> [Map String t]

    -- |Удалить правило из базы знаний
    retract :: k p t -> p -> k p t

    -- |Список правил
    axioms :: k p t -> [p]

    -- |Проверить, содержится ли правило в базе знаний
    contains :: k p t -> p -> Bool
    contains kb p = p `elem` axioms kb
