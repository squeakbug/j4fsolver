module LibSolver.CSP.AC3 where

import Control.Monad (when, forM_, unless)
import Data.Map ((!))

import LibSolver.CSP
import LibSolver.Types.Queue
import LibSolver.Util

import qualified Data.Map as M
import qualified Data.List as L

---------------------------------------------------------------------------------

-- |The arc-consistency algorithm AC-3 to reduce the domains of a constraint
--  satisfaction problem until they are arc-consistent. There is no return
--  value. This function is only called for its effects on the current domain.
ac3 :: (CSP c v a, Queue q) =>
       c v a                -- ^ CSP
    -> q (v,v)              -- ^ Variables to be tested
    -> Backtracking v a ()  -- ^ Restricted domain or Nothing
ac3 csp queue = when (notEmpty queue) $
    do revised <- removeInconsistentValues csp x y
       if not revised
          then ac3 csp rest
          else do dom <- getDomain
                  when (notNull $ dom ! x) (ac3 csp queue')
    where
        ((x,y), rest) = pop queue
        queue'        = extend new rest
        new           = [ (z,x) | z <- L.delete y (neighbours csp ! x) ]

-- |Remove values for a variable that are inconsistent with the constraints.
--  Returns a 'Bool' flag indicating whether the domain has been revised or not.
--  An /inconsistent/ value for @x@ is one for which we can't find any value in
--  the domain of @y@ such that the constraints are satisfied.
removeInconsistentValues :: CSP c v a =>
                            c v a   -- ^ CSP
                         -> v       -- ^ Var to restrict
                         -> v       -- ^ Var to check against
                         -> Backtracking v a Bool
removeInconsistentValues csp x y = getDomain >>= \dom -> do

    let old = dom ! x
        new = filter fun old
        fun xv = any (constraints csp x xv y) (dom ! y)

    if length new < length old
        then modifyDomain (M.insert x new) >> return True
        else return False

-- |Add (v, a) to a map of current assignments, discarding the old
--  aue if any. Also update the current domain if necessary.
assign :: CSP c v a => c v a -> v -> a -> Backtracking v a ()
assign csp var val = do
    modifyAssignment (M.insert var val)
    whenM useFc  $ forwardCheck csp var val
    whenM useMac $ ac3 csp [ (x,var) | x <- neighbours csp ! var ]

-- |Remove (v, a) from assignments, i.e. backtrack. Do not call this
--  if you are assigning v to a new value - just call 'assign' for that.
--  Also resets the current domain for this variable to the full domain
--  allowed by the CSP.
unassign :: CSP c v a => c v a -> v -> Backtracking v a ()
unassign csp var = do
    modifyAssignment $ M.delete var
    modifyDomain     $ M.insert var (domains csp ! var)

-- |Do forward checking (current domain reduction) for a (var, val) pair.
forwardCheck :: CSP c v a => c v a -> v -> a -> Backtracking v a ()
forwardCheck csp var val = do
    pruned <- getPruned
    assgn  <- getAssignment
    -- Restore prunings from previous value of var.
    forM_ (pruned ! var) $ \(x,y) -> modifyDomain (restore x y)
    -- Remove all prunings.
    putPruned (mkUniversalMap (vars csp) [])
    -- Prune any other assignment that conflicts with var = val.
    forM_ (neighbours csp ! var) $ \x -> when (x `M.notMember` assgn) $
        do dom <- getDomain
           forM_ (dom ! x) $ \y -> unless (constraints csp var val x y) $
                do modifyDomain (prune y)
                   modifyPruned (add x y)
    where
        restore x y = M.adjust (y:) x
        prune y     = M.adjust (L.delete y) var
        add x y     = M.adjust ((x,y):) var
