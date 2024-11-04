module LibSolver.SemiRing where

import Data.Set (Set)

class SemiRing a where
  -- | Addition
  (<+>) :: a -> a -> a
  -- | Multiplication
  (<*>) :: a -> a -> a
  -- | Compute additive inverse
  neg   :: a -> a
  -- | The additive identity
  zero  :: a
  -- | The multiplicative identity
  one   :: a

-----------------------------------------------------------------------------

data BoolOp = BoolNullary Bool
            | BoolUnary  (Bool -> Bool)
            | BoolBinary (Bool -> Bool -> Bool)

data BoolSemiRing = BoolSemiRing
    { domain     :: Set Bool
    , operations :: Set BoolOp
    }

-- data BoolBasis = BoolBasis
