-----------------------------
-- Compare Graph Searchers --
-----------------------------

-- |Run all search algorithms over a particular problem and print out
--  performance statistics.
runDetailedCompare :: (Problem p s a, Ord s, Show s) => p s a -> IO ()
runDetailedCompare = detailedCompareSearchers allSearchers allSearcherNames

-- |List of all search algorithms that can be applied to problems with a graph
--  structure. I'd like to add an iterative deepening graph search to this list,
--  as well as some of the more exotic search algorithsm described in the
--  textbook.
allSearchers :: (Problem p s a, Ord s) => [p s a -> Maybe (Node s a)]
allSearchers = [ breadthFirstGraphSearch, depthFirstGraphSearch
               , greedyBestFirstSearch, uniformCostSearch, aStarSearch']

-- |Names for the search algorithms in this module.
allSearcherNames :: [String]
allSearcherNames = [ "Breadth First Graph Search" , "Depth First Graph Search"
                   , "Greedy Best First Search", "Uniform Cost Search"
                   , "A* Search"]