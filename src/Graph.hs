module Graph (
    Vertex,
    Graph,
) where

import SearchState (SearchState)

data Vertex a = Vertex {
      vertexLabel :: [Char]
    , verterState :: SearchState a
    , vertexNeighbors :: [[Char]]
    , vertexDistance :: Int
    , vertexPredecessor :: [Char]
} deriving (Show)

-- Определяем граф, как список вершин.
-- Каждая вершина - это метка, список смежности (соседи),
-- расстояние от корня и метка-предшественник.
data Graph a = Graph [Vertex a] deriving (Show)

-- Ребро
type Edge = ([Char], [Char])

-- Дуга
type Arc = ([Char], [Char]) 

-----------------------------------------------------------------------------

vertices :: Graph -> [Vertex]
vertices g = []

edges :: Graph -> [Edge] 
edges g = []

-----------------------------------------------------------------------------

parseEdge :: String -> ([Char], [Char])
parseEdge line = let [from, to] = map read (splitOn "->" line) in (from, to)

parseGraph :: [Char] -> Graph
parseGraph contents =
    let edges = map parseEdge (lines contents)
    return (buildG (0, maximum (map snd edges)) edges)

serializeGraph :: FilePath -> Graph -> IO ()
serializeGraph filePath graph = do
    writeFile filePath (unlines [show from ++ " -> " ++ show to | (from, to) <- edges graph])

outputGraphviz :: FilePath -> Graph -> IO ()
outputGraphviz filePath graph =
    let graphvizEdges = unlines ["  " ++ show from ++ " -> " ++ show to ++ ";" | (from, to) <- edges graph]
    in writeFile filePath $ "digraph G {\n" ++ graphvizEdges ++ "\n}"
