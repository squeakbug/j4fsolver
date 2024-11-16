module LibSolver.CSP where

import System.Random
import Control.Monad.Reader
import Control.Monad.State
import Data.Map (Map, (!))
import Data.Functor


import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Ord as O
import LibSolver.Util

-- |Data type used for domains.
type Domain a b = Map a [b]

-- |Data type used for neighbor lists.
type Neighbour a = Map a [a]

-- |Data type used for assignments.
type Assignment a b = Map a b

-- |This class describes finite-domain Constraint Satisfaction Problems.
--  A CSP is specified by the following three inputs:
--
--  * vars        A list of variables; each is atomic (eg Int or String)
--
--  * domains     A map of (var, val) entries
--
--  * neighbours  A map of (var, [var]) that for each variable lists the
--                other variables that participate in the constraints.
--
--  * constraints A function @f A a B b@ that returns @True@ if neighbours A, B
--                satisfy the constraint when @A == a@ and @B == b@.
--
--  In the textbook and in most mathematical definitions, the constraints are
--  specified as explicit pairs of allowable values, but the formulation here
--  is easier to express and more compact in most cases. (For example, the
--  n-Queens problem can be represented in O(n) space using this notation,
--  instead of O(n^4) for the explicit representation.)
--
--  The class also supports data structures and methods that help you
--  solve CSPs by calling a search function on the CSP.
class (Ord v, Eq a) => CSP c v a where

    -- |A list of viables.
    vars :: c v a -> [v]

    -- |A mapping of viables to possible aues.
    domains :: c v a -> Domain v a

    -- |A mapping of viables to a list of the other viables that
    --  participate in its constraints.
    neighbours :: c v a -> Neighbour v

    -- |A function @f A a B b@ that returns True if neighbours A, B satisfy
    --  the constraint when they have aues A == a and B == b.
    constraints :: c v a -> v -> a -> v -> a -> Bool

-----------------------
-- Utility Functions --
-----------------------

-- |Return a list of variables in the current assignment that are in conflict.
conflictedVars :: CSP c v a => c v a -> Assignment v a -> [v]
conflictedVars csp a =
    [ v | v <- vars csp, hasConflicts csp v (a ! v) a ]

-- |Check if an assignment is complete, i.e. there are no more viables
--  left to assign.
allAssigned :: CSP c v a => c v a -> Assignment v a -> Bool
allAssigned csp assignment = M.size assignment == length (vars csp)

hasConflicts :: CSP c v a => c v a -> v -> a -> Assignment v a -> Bool
hasConflicts  csp v a  = not . M.null . M.filterWithKey conflict
    where
        conflict x y = not $constraints csp v a x y

-- |Return the number of conflicts that v == a has with other
--  viables currently assigned.
numConflicts :: CSP c v a => c v a -> v -> a -> Assignment v a -> Int
numConflicts csp v a = M.foldrWithKey count 0
    where
        ok = constraints csp v a
        count k v' n  = if ok k v' then n else n+1


-- |The goal is to assign all vars with all constraints satisfied.
goalTest :: CSP c v a => c v a -> Assignment v a -> Bool
goalTest csp assignment =
    allAssigned csp assignment && all noConflicts (vars csp)
    where
        noConflicts var = not $
            hasConflicts csp var (assignment ! var) assignment

-- |Options for recursive backtracking. We allow the following options:
--  
--  * 'mcv' - use the Most Constrained Variable heuristic
--
--  * 'lcv' - use the Least Constraining Variable heuristic
--
--  * 'fc'  - use Forward Checking
--
--  * 'mac' - use Maintaining Arc Consistency
data Opts = Opts
    { mcv :: Bool
    , lcv :: Bool
    , fc  :: Bool
    , mac :: Bool }

-- |Default options for backtracking search. Doesn't use any heuristics.
defaultOpts :: Opts
defaultOpts = Opts False False False False

-- |Use 'optimized' options for backtracking search. We maintain arc consistency
--  at each stage, and use the most constrained variable and least constraining
--  variable heuristics.
fastOpts :: Opts
fastOpts = Opts True True False True

-- |Use Most Constrained Variable heuristic?
useMcv :: MonadReader Opts m => m Bool
useMcv = asks mcv

-- |Use Least Constraining Variable heuristic?
useLcv :: MonadReader Opts m => m Bool
useLcv = asks lcv

-- |Use Forwarc Checking?
useFc  :: MonadReader Opts m => m Bool
useFc = asks fc

-- |Use Maintaining Arc Consistency?
useMac :: MonadReader Opts m => m Bool
useMac = asks mac

-- |State variables for search in the 'Backtracking' monad.
type BTState a b = (Domain a b, Map a [(a,b)], Assignment a b)

-- |Monad for backtracking search. We use a @StateT@ monad to keep track of the
--  current domain, pruned values and assigned values, and wrap a @Reader Opts@
--  monad to keep track of the various search options.
type Backtracking a b c = StateT (BTState a b) (Reader Opts) c

-- |Use this to run a computation in the 'Backtracking' monad to extract
--  the final state and value.
runBacktracking :: Ord a => Backtracking a b c -> Domain a b -> Opts -> (c, BTState a b)
runBacktracking computation dom = runReader (runStateT computation (dom, prune, assgn))
    where
        prune = mkUniversalMap (M.keys dom) []
        assgn = M.empty

-- |Use this to evaluate the result of computation in the 'Backtracking' monad.
evalBacktracking :: Ord a => Backtracking a b c -> Domain a b -> Opts -> c
evalBacktracking c dom opts = fst $ runBacktracking c dom opts

-- |Use this to evaluate the final state of a computation in the 'Backtracking'
--  monad.
execBacktracking :: Ord a => Backtracking a b c -> Domain a b -> Opts -> BTState a b
execBacktracking c dom opts = snd $ runBacktracking c dom opts

-- |Return the current (constrained) domains in backtracking search.
getDomain :: MonadState (a,b,c) m => m a
getDomain = gets fst3

-- |Modify the current constrained domains in backtracking search.
modifyDomain :: MonadState (a,b,c) m => (a -> a) -> m ()
modifyDomain f = modify $ \(x,y,z) -> (f x,y,z)

-- |Return the list of pruned variables in backtracking search.
getPruned :: MonadState (a,b,c) m => m b
getPruned = gets snd3

-- |Store a new set of pruned values
putPruned :: MonadState (a,b,c) m => b -> m ()
putPruned p = get >>= \(x,_,z) -> put (x,p,z)

-- |Modify the list of pruned variables in backtracking search.
modifyPruned :: MonadState (a,b,c) m => (b -> b) -> m ()
modifyPruned f = modify $ \(x,y,z) -> (x,f y,z)

-- |Return the current assignment list in backtracking search.
getAssignment :: MonadState (a,b,c) m => m c
getAssignment = gets thd3

-- |Store a new assignment in place of the old one.
putAssignment :: MonadState (a,b,c) m => c -> m ()
putAssignment a = get >>= \(x,y,z) -> put (x,y,a)

-- |Modify the list of assignments in backtracking search.
modifyAssignment :: MonadState (a,b,c) m => (c -> c) -> m ()
modifyAssignment f = modify $ \(x,y,z) -> (x,y,f z)
