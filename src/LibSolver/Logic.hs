module LibSolver.Logic where

import Data.Set (Set)
import Data.Text (Text)

import LibSolver.BoolExpr

-- |База знаний
--  p - тип правила
--  t - тип таблицы (определяет процедуру поиска, разрешения, добавления и удаления правил)
class (Expr p, Show t) => KB k p t where
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

-----------------------------------------------------------------------------

newtype PropositionForm a = PropositionForm
    { expr :: BoolExpr a
    }

data PropositionRule a = PropositionRule
    { antecedent :: [PropositionForm a] -- Предусловие
    , consequent :: PropositionForm a   -- Следствие
    }

data Theory a = Theory
    { vars   :: Set Text                -- Множество термов
    , axioms :: Set (PropositionForm a) -- Аксиомы
    , rules  :: Set (PropositionRule a) -- Правила вывода
    }

data Solver
