module LibSolver.CSP.Backtracking where

import Control.Monad (when)
import Data.Map ((!))
import System.Random

import LibSolver.CSP
import LibSolver.Types.Queue
import LibSolver.Util

import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Ord as O
import LibSolver.CSP.AC3 (assign, unassign)

-- |Backtracking search. This is a wrapper for 'recursiveBacktracking'. You
--  need to supply the problem to be solved, and a set of options. The easiest
--  (but slowest) method is to call this function with 'defaultOpts'.
backtrackingSearch :: CSP c var val =>
                      c var val     -- ^ Constraint Satisfaction Problem
                   -> Opts          -- ^ Search options
                   -> Maybe (Assignment var val)
backtrackingSearch csp = evalBacktracking (recursiveBacktracking csp) (domains csp)

-- |Recursive backtracking search. This is the main workhorse. We make use of
--  the 'Backtracking' monad, which stores the current variable assignments,
--  the current domain and a list of pruned values, as well as a list of
--  options for the search.
recursiveBacktracking :: CSP c v a =>
                         c v a
                      -> Backtracking v a (Maybe (Assignment v a))
recursiveBacktracking csp = getAssignment >>= \assgn ->
    if allAssigned csp assgn
        then return (Just assgn)
        else do
            var  <- selectVariable (vars csp)
            vals <- sortDomainVals csp var
            go var vals
        where
            go var []     = return Nothing
            go var (v:vs) = getAssignment >>= \assgn ->
                if noConflicts var v assgn
                    then do assign csp var v
                            result <- recursiveBacktracking csp
                            if no result
                                then unassign csp var >> go var vs
                                else return result
                    else go var vs

            noConflicts var v a = not $ hasConflicts csp var v a

-- |Select an unassigned variable from the list of variables in a CSP. We may
--  optionally use the Most Constrained Variable heuristic to choose which
--  variable to assign next.
selectVariable :: Ord v => [v] -> Backtracking v a v
selectVariable vs = ifM useMcv (mostConstrained vs) (firstUnassigned vs)

-- |Given a variable in a CSP, select an order in which to try the allowed
--  values for that variable. We may optionally use the Least Constraining
--  Variable heuristic to try values with fewest conflicts first.
sortDomainVals :: CSP c v a => c v a -> v -> Backtracking v a [a]
sortDomainVals csp var = ifM useLcv (leastConstraining csp var) (allValues var)

-- |Return the most constrained variable from a problem. The idea is to speed
--  up the search algorithm by reducing the branching factor.
mostConstrained :: Ord v => [v] -> Backtracking v a v
mostConstrained vs = do
    dom   <- getDomain
    assgn <- getAssignment
    let unassigned = [ v | v <- vs, v `M.notMember` assgn ]
    return $! argMin unassigned (numLegalValues dom)

-- |Return the first unassigned variable in the problem.
firstUnassigned :: Ord v => [v] -> Backtracking v a v
firstUnassigned (v:vs) = getAssignment >>= select vs
    where
        select vs assgn = if v `M.notMember` assgn
            then return v
            else firstUnassigned vs

-- |Return a list of values for a given variable, sorted in order of least
--  constraining to most constraining.
leastConstraining :: CSP c v a => c v a -> v -> Backtracking v a [a]
leastConstraining csp var = do
    dom   <- getDomain
    assgn <- getAssignment
    return . sortWith (numConf assgn)  $! (dom ! var)
    where
        numConf a val = numConflicts csp var val a
        sortWith :: Ord b => (a -> b) -> [a] -> [a]
        sortWith f = map snd. L.sortBy (O.comparing fst) . map (\x -> (f x, x))

-- |Return a list of all possible values for a given variable, without doing
--  any sorting.
allValues :: Ord v => v -> Backtracking v a [a]
allValues var = getDomain >>= \dom -> return $ dom ! var

-- |Return the number of legal values for a variable.
numLegalValues :: Ord v => Domain v a -> v -> Int
numLegalValues dom var = length (dom ! var)
