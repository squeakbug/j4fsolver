{-# LANGUAGE DatatypeContexts #-}

module LibSolver.Search.Dfs
    ( dfsFromGoal
    , dfsFromPredicate
    ) where

import LibSolver.DiGraph
import LibSolver.Vertex
import LibSolver.Search (SearchResult(..))

data (DiGraph dg a) => DFS dg a = DFS
    { graph      :: dg               -- Исходный граф
    , isFinal    :: Vertex a -> Bool -- Предикат: вершина является целевой
    , stack      :: [Vertex a]       -- Стек
    , seen       :: [Vertex a]       -- Просмотренные вершины
    , path       :: [Vertex a]       -- Путь от корня до цели
    , isFinished :: Bool
    }

fromGoal :: (DiGraph dg a, Eq a) => dg -> Vertex a -> Vertex a -> DFS dg a
fromGoal g goal initV = DFS
    { graph = g
    , isFinal = (== goal)
    , stack = [initV]
    , seen  = []
    , path  = []
    , isFinished = False
    }

fromPredicate :: (DiGraph dg a) => dg -> (Vertex a -> Bool) -> Vertex a -> DFS dg a
fromPredicate g isGoal initV = DFS
    { graph = g
    , isFinal = isGoal
    , stack = [initV]
    , seen  = []
    , path  = []
    , isFinished = False
    }

fromDFS :: DiGraph dg a => DFS dg a -> SearchResult a
fromDFS DFS
    { path=path
    , stack=(v:_)
    } = SearchResult
    { finalPath = path
    , finalVertex = Just v
    , isFinded = True
    }

fromDFS DFS
    { path=path
    , stack=[]
    } = SearchResult
    { finalPath = path
    , finalVertex = Nothing
    , isFinded = False
    }

-----------------------------------------------------------------------------

-- Удаление из списка (2 аргумент) уже просмотренных вершин (1 аргумент)
filterVertexNeighbors :: Eq a => [Vertex a]   ->  [Vertex a]  -> [Vertex a]
filterVertexNeighbors _ [] = []
filterVertexNeighbors [] xs = xs
filterVertexNeighbors s vn = filter (`notElem` s) vn

-- Стек пуст, а вершина не найдена -> заканчиваем поиск
dfsStep :: (DiGraph dg a, Eq a) => DFS dg a -> DFS dg a
dfsStep d@DFS
    { stack=[]
    } = d
    { isFinished=True
    }

-- Искомая вершина найдена
dfsStep d@DFS
    { isFinal=isFinal
    , stack=(v:_)
    } | isFinal v = d
    { isFinal=isFinal
    , isFinished=True
    }

-- Иначе, если в очереди остались вершины
dfsStep d@DFS
    { graph=g
    , stack=(v:f)
    , seen=seen
    , path=path
    } = d
    { graph=g
    , stack=stack'
    , seen=seen'
    , path=path'
    , isFinished=False
    }
    where vNeighbours = giVertexNeighbors g v
          notSeenNeighbours = filterVertexNeighbors seen vNeighbours
          stack' = notSeenNeighbours ++ f 
          seen' = v : seen
          path' = v : path

dfsHelper :: (DiGraph dg a, Eq a) => DFS dg a -> DFS dg a
dfsHelper initState =
    let nextStep = dfsStep initState
    in (if isFinished nextStep then nextStep else dfsHelper nextStep)

-----------------------------------------------------------------------------

dfsFromGoal :: (DiGraph dg a , Eq a) => dg -> Vertex a -> Vertex a -> SearchResult a
dfsFromGoal g fin initV = fromDFS $ dfsHelper $ fromGoal g fin initV

dfsFromPredicate :: (DiGraph dg a, Eq a) => dg -> (Vertex a -> Bool) -> Vertex a -> SearchResult a
dfsFromPredicate g fin initV = fromDFS $ dfsHelper $ fromPredicate g fin initV
