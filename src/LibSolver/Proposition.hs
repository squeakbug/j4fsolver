module LibSolver.Proposition where

import Data.Set (Set)
import Data.Text (Text)

import LibSolver.BoolExpr

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
