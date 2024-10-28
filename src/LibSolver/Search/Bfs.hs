{-# LANGUAGE DatatypeContexts #-}

module LibSolver.Search.Bfs
    ( bfsFromGoal
    , bfsFromPredicate
    ) where

import LibSolver.DiGraph
import LibSolver.Vertex

data (DiGraph dg) => BFS dg a = BFS
    { graph      :: dg               -- Исходный граф
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

fromGoal :: (DiGraph dg, Eq a) => dg -> Vertex a -> Vertex a -> BFS dg a
fromGoal g goal initV = BFS
    { graph = g
    , isFinal = (== goal)
    , queue = [initV]
    , seen  = []
    , path  = []
    , isFinished = False
    }

fromPredicate :: (DiGraph dg) => dg -> (Vertex a -> Bool) -> Vertex a -> BFS dg a
fromPredicate g isGoal initV = BFS
    { graph = g
    , isFinal = isGoal
    , queue = [initV]
    , seen  = []
    , path  = []
    , isFinished = False
    }

fromBFS :: DiGraph dg => BFS dg a -> BFSResult a
fromBFS BFS
    { path=path
    , queue=(v:_)
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
bfsStep :: DiGraph dg => BFS dg a -> BFS dg a
bfsStep BFS
    { graph=g
    , isFinal=isFinal
    , queue=[]
    , seen=seen
    , path=path
    , isFinished=_
    } = BFS
    { graph=g
    , isFinal=isFinal
    , queue=[]
    , seen=seen
    , path=path
    , isFinished=True
    }

-- Искомая вершина найдена
bfsStep BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue@(v:_)
    , seen=seen
    , path=path
    , isFinished=_
    } | isFinal v = BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue
    , seen=seen
    , path=path
    , isFinished=True
    }

-- Иначе, если в очереди остались вершины
bfsStep BFS
    { graph=g
    , isFinal=isFinal
    , queue=(v:f)
    , seen=seen
    , path=path
    , isFinished=_
    } = BFS
    { graph=g
    , isFinal=isFinal
    , queue=queue'
    , seen=seen'
    , path=path'
    , isFinished=False
    }
    where vNeighbours = giVertexNeighbors g v
          notSeenNeighbours = filterVertexNeighbors seen vNeighbours
          queue' = f ++ notSeenNeighbours
          seen' = v : seen
          path' = v : path

bfsHelper :: DiGraph dg => BFS dg a -> BFS dg a
bfsHelper initState =
    let nextStep = bfsStep initState
    in (if isFinished nextStep then nextStep else bfsHelper nextStep)

-----------------------------------------------------------------------------

bfsFromGoal :: (DiGraph dg , Eq a) => dg -> Vertex a -> Vertex a -> BFSResult a
bfsFromGoal g fin initV = fromBFS $ bfsHelper $ fromGoal g fin initV

bfsFromPredicate :: (DiGraph dg) => dg -> (Vertex a -> Bool) -> Vertex a -> BFSResult a
bfsFromPredicate g fin initV = fromBFS $ bfsHelper $ fromPredicate g fin initV
