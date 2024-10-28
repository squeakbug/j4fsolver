module LibSolver.Search.Bfs
    ( bfsFromGoal
    , bfsFromPredicate
    ) where

import Codec.Binary.UTF8.Generic (UTF8Bytes(empty))

import LibSolver.DiGraph
import LibSolver.Vertex

data BFS a = BFS
    { graph      :: DiGraph a        -- Исходный граф
    , isFinal    :: Vertex a -> Bool -- Предикат: вершина является целевой
    , queue      :: [Vertex a]       -- Очередь
    , seen       :: [Vertex a]       -- Просмотренные вершины
    , path       :: [Vertex a]       -- Путь от корня до цели
    , isFinished :: Bool
    }

data BFSResult a = BFSResult
    { finalPath    :: [Vertex a]       -- Путь от корня до цели
    , finalVertex  :: Maybe (Vertex a) -- Конечная вершина
    , isFinded     :: Bool             -- Была ли найдена вершина
    }

fromGoal :: (Eq a) => DiGraph a -> Vertex a -> Vertex a -> BFS a
fromGoal g goal init = BFS
    { graph = g
    , isFinal = (== goal)
    , queue = [init]
    , seen  = []
    , path  = []
    }

fromPredicate :: DiGraph a -> (Vertex a -> Bool) -> Vertex a -> BFS a
fromPredicate g isGoal init = BFS
    { graph = g
    , isFinal = isGoal
    , queue = [init]
    , seen  = []
    , path  = []
    }

fromBFS :: BFS a -> BFSResult a
fromBFS BFS
    { path=path
    , queue=(v:f)
    } = BFSResult
    { finalPath = path
    , finalVertex = Just v
    , isFinded = True
    }

fromBFS BFS
    { path=path
    , queue=[]
    } = BFSResult
    { finalPath = path
    , finalVertex = Nothing
    , isFinded = False
    }

-----------------------------------------------------------------------------

-- Проверка на наличие вершины в списке вершин на основе метки
vertexInVertexes :: Vertex a -> [Vertex a] -> Bool
vertexInVertexes _ [] = False
vertexInVertexes Vertex { vertexLabel = label } (x:y) =
    foldl (\acc x' -> vertexLabel x' == label || acc) False (x:y)

-- Удаление из списка (2 аргумент) уже просмотренных вершин (1 аргумент)
filterVertexNeighbors :: [Vertex a]   ->  [Vertex a]  -> [Vertex a]
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] _ = []
filterVertexNeighbors s vn = filter (\x -> not $ vertexInVertexes x s) vn

-- Очередь пуста, а вершина не найдена -> заканчиваем поиск
bfsStep :: BFS a -> BFS a
bfsStep BFS
    { graph=g
    , isFinal=isFinal
    , queue=[]
    , seen=seen
    , path=path
    , isFinished=isFinished
    } = BFS
    { graph=g
    , isFinal=isFinal
    , queue=[]
    , seen=seen
    , path=path
    , isFinished=True
    }

-- Искомая вершина найдена
bfsStep b@BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue@(v:f)
    , seen=seen
    , path=path
    , isFinished=isFinished
    } | isFinal v = BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue
    , seen=seen
    , path=path
    , isFinished=True
    }

-- Иначе, если в очереди остались вершины
bfsStep b@BFS
    { graph=g
    , isFinal=isFinal
    , queue=(v:f)
    , seen=seen
    , path=path
    , isFinished=isFinished
    } = BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue'
    , seen=seen'
    , path=path'
    , isFinished=False
    }
    where vNeighbours = _vertexNeighbours g v
          notSeenNeighbours = filterVertexNeighbors seen vNeighbours
          queue' = f ++ notSeenNeighbours
          seen' = v : seen
          path' = v : path

bfsHelper :: BFS a -> BFS a
bfsHelper initState =
    let nextStep = bfsStep initState
    in (if isFinished nextStep then nextStep else bfsHelper nextStep)

-----------------------------------------------------------------------------

bfsFromGoal :: (Eq a) => DiGraph a -> Vertex a -> Vertex a -> BFSResult a
bfsFromGoal g fin init = fromBFS $ bfsHelper $ fromGoal g fin init

bfsFromPredicate :: DiGraph a -> (Vertex a -> Bool) -> Vertex a -> BFSResult a
bfsFromPredicate g fin init = fromBFS $ bfsHelper $ fromPredicate g fin init
