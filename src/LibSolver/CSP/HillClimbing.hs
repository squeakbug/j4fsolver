module LibSolver.CSP.HillClimbing where

import Control.Monad (when)
import Data.Map ((!))
import System.Random

import LibSolver.CSP
import LibSolver.Types.Queue
import LibSolver.Util

import qualified Data.Map as M
import qualified Data.List as L

---------------------------------------------------------------------------------

-- |Solve a Constraint Satisfaction Problem by stochastic hillclimbing on the
--  number of conflicts.
minConflictsIO :: CSP c v a => c v a -> Int -> IO (Assignment v a)
minConflictsIO csp maxSteps = do
    g <- getStdGen
    return (minConflicts g csp maxSteps)

-- |Solve a Constraint Satisfaction Problem by stochastic hillclimbing on the
--  number of conflicts. This is the /pure/ version of the algorithm. You must
--  supply a random number generator. See also 'minConflictsIO'.
minConflicts :: (CSP c v a, RandomGen g) =>
                g
             -> c v a
             -> Int
             -> Assignment v a
minConflicts gen csp maxSteps = go g 0 initial
    where
        (initial, g) = initialAssignment gen csp

        go g steps current =
            let conflicted = conflictedVars csp current
             in if steps == maxSteps || null conflicted
                    then current 
                    else let (var, g1) = randomChoice g conflicted
                             (val, g2) = minConflictsValue g1 csp var current
                          in go g2 (steps+1) (M.insert var val current)

-- |The initial assignment for the min-conflicts algorithm. We choose the
--  assignments according to the minimum-conflicts heuristic, breaking ties
--  at random.
initialAssignment :: (CSP c v a, RandomGen g) => g -> c v a -> (Assignment v a, g)
initialAssignment g csp = go g (vars csp) M.empty
    where
        go g []         current = (current, g)
        go g (var:rest) current = 
            let (val, g') = minConflictsValue g csp var current
             in go g' rest (M.insert var val current)
            

-- |Return the value that will give a variable the least number of conflicts.
--  If there is a tie, choose at random.
minConflictsValue :: (CSP c v a, RandomGen g) => g -> c v a -> v -> Assignment v a -> (a, g)
minConflictsValue g csp var current =
    argMinRandom g (domains csp ! var) (\v -> numConflicts csp var v current)