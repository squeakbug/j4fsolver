module LibSolver.Search.Bfs
    ( bfsFromGoal
    , bfsFromPredicate
    ) where

import LibSolver.Graph

data BFS a = BFS
    { isFinal :: Vertex a -> Bool -- Предикат: вершина является целевой
    , queue   :: [Vertex a]       -- Очередь
    , seen    :: [Vertex a]       -- Просмотренные вершины
    , path    :: [Vertex a]       -- Путь от корня до цели
    }

data BFSResult a = BFSResult
    { finalPath    :: [Vertex a] -- Путь от корня до цели
    , finalVertex  :: Vertex a   -- Конечная вершина
    }

fromGoal :: Vertex a -> BFS a
fromGoal goal = BFS
    { isFinal = (== goal)
    , queue = []
    , seen  = []
    , path  = []
    }

fromPredicate :: (Vertex a -> Bool) -> BFS a
fromPredicate p = BFS
    { isFinal = p
    , queue = []
    , seen  = []
    , path  = []
    }

fromBFS :: BFS a -> Maybe (BFSResult a)
fromBFS BFS
    { queue=_
    , seen=_
    , path=path
    } = Just BFSResult
    { finalPath = path
    , finalVertex = head path
    }

-----------------------------------------------------------------------------

-- Проверка на наличие вершины в списке вершин на основе метки
vertexInVertexes :: Vertex a -> [Vertex a] -> Bool
vertexInVertexes _ [] = False
vertexInVertexes Vertex { vertexLabel = label } (x:y) = 
    foldl (\acc x' -> vertexLabel x' == label || acc) False (x:y)

-- Принимает граф, список меток и выдает список вершин.
graphVertexes :: Graph a -> [[Char]]-> [Vertex a]
graphVertexes (Graph []) _ = []
graphVertexes (Graph (x:y)) [] = x : y
-- Метка вершины - это элемент в списке меток (ключей).
graphVertexes (Graph (x:y)) keys = filter (\z -> vertexLabel z `elem` keys) (x:y)

-- Удаление из списка (2 аргумент) уже просмотренных вершин (1 аргумент)
filterVertexNeighbors :: [Vertex a]   ->  [Vertex a]  -> [Vertex a]
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] _ = []
filterVertexNeighbors s vn = filter (\x -> not $ vertexInVertexes x s) vn

-- Обновить статистику вершин
updateDistPred :: [Vertex a] -> Int -> [Char] -> [Vertex a]
updateDistPred [] _ _ = []
updateDistPred (x:y) dist predLabel = 
    map (\(Vertex 
        { vertexLabel = label
        , vertexState = s
        , vertexNeighbors = ns
        }
    ) -> Vertex 
        { vertexLabel = label
        , vertexState = s
        , vertexNeighbors = ns
        , vertexDistance = dist
        , vertexPredecessor = predLabel
        }) (x:y)

bfsHelper :: BFS a -> BFS a
bfsHelper BFS
    { queue=queue@(e:f)
    , seen=seen
    , path=path
    } = BFS
    { queue=queue'
    , seen=seen'
    , path=path'
    }
    where lst = last queue
          eLabel = vertexLabel e
          eNeighbors = 
          eVertexNeighbors = graphVertexes inGraph eNeighbors
    

bfsHelper b = b

bfsFromGoal :: Vertex a -> Maybe (BFSResult a)
bfsFromGoal = fromBFS . bfsHelper . fromGoal

bfsFromPredicate :: (Vertex a -> Bool) -> Maybe (BFSResult a)
bfsFromPredicate = fromBFS . bfsHelper . fromPredicate
