module LibSolver.Graph.Parser
    ( parseGraph
    ) where

import LibSolver.Graph

parseArc :: String -> Arc
parseArc line = let [from, to] = map (trim . read) (splitOn "->" line) in Arc 
    { from
    , to
    }

parseVertexNeighbors :: [Char] -> [[Char]]
parseVertexNeighbors [] = []
parseVertexNeighbors ns = map trim $ splitOn "," ns

parseVertexData :: (SearchState a, Read a) => [Char] -> ([Char], a)
parseVertexData line = let [label, dt] = splitOn ":" line in (trim label, read dt)

parseLine :: [Char] -> ([Char], [[Char]])
parseLine line = let [v, ns] = map trim $ splitOn "->" line in (v, parseVertexNeighbors ns)

buildVertex :: (SearchState a, Read a) => [Char] -> a -> Vertex a
buildVertex label d = Vertex 
    { vertexLabel = label
    , vertexState = d
    }

buildGraph :: (SearchState a, Read a) => [Vertex a] -> (VertexLabel -> [VertexLabel]) -> Graph a
buildGraph = Graph

parseGraph :: (SearchState a, Read a) => [Char] -> Graph a
parseGraph contents =
    buildGraph $ zipWith (\(v, ns) (_label, d) -> buildVertex v ns d) pvds pds
    where
        [vds, ds] = splitOn "---" contents
        pvds = map parseLine $ lines vds
        pds = map parseVertexData (splitOn "\n\n" ds)