{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving  #-}

module LibSolver.Vertex 
    ( Vertex(..)
    , VertexLabel
    ) where

import Data.Text (Text)

import LibSolver.Search.SearchState (SearchState)

-- |Узел в дереве поиска. 
--  Обратите внимание, что если к состоянию можно прийти двумя путями, то будет два узла с одинаковым состоянием. 
data Node s a = Node 
    { state  :: s                -- Состояние узла
    , parent :: Maybe (Node s a) -- Cсылка на родителя (узел, преемником которого он является)
    , action :: Maybe a          -- (Optional) Действие, которое привело к этому состоянию
    , cost   :: Cost             -- (Optional) Стоимость пути до данного узла
    , depth  :: Int              -- Количество вершин от корня до узла
    , value  :: Double           -- Стоимость вершины
    }

instance (Show s, Show a) => Show (Node s a) where
    show (Node state _ action cost depth _) =
        "Node(state=" ++ show state ++ ",action=" ++ show action ++ 
            ",cost=" ++ show cost ++ ",depth=" ++ show depth ++ ")"

root :: (Problem p s a) => p s a -> Node s a
root p = Node s Nothing Nothing 0 0 (valueP p s) where s = initial p

path :: Node s a -> [Node s a]
path n = case parent n of
    Nothing -> [n]
    Just n' -> n : path n'

-- |Возвращает список узлов, достижимых из данного узла в контексте указанной проблемы.
expand :: (Problem p s a) => p s a -> Node s a -> [Node s a]
expand p node = [ mkNode a s | (a,s) <- successor p (state node) ]
    where
        mkNode a s = Node s (Just node) (Just a) (c a s) (1 + depth node) v
        c      a s = costP p (cost node) (state node) a s
        v          = valueP p (state node)

---------------------------------------------------------------------------------

-- QUESTION: В каких системах операции 'сравнить два состояния' и 'определить, является ли данное состояние конечным' имеют разные стоимости

-- TODO: Привести реализации типажа для часто встречаемых объектов:
-- 1) преставители группы перестановок
-- 2) массивы с постоянным количеством пропусков (пятнашки)
-- 3) массивы с уменьшающимся после применения метода `produce` количеством пропусков (судоку)
-- 4) массивы с недетерминированым количеством пропусков (с заданной вероятностью увеличение или уменьшение)
-- 5) конечные автоматы (лексеры, протоколы взаимодействия конечного числа акторов)
-- 6) автоматы с магазинной памятью (парсеры КС-грамматик)
-- 7) тьюринг-полные вычислители (алгоритмы, символьное исполнение)

-- ?) reward model
-- ?) система с подсказками (подсказки не используются при построении графа, но используются при поиске в пространстве состояний)
-- ?) система с возможностью пройти одно и то же состояние в процессе поиска конечное число раз
 
-- Типаж представителя пространства состояний со взвешанными дугами
-- class (PartialEq a, Ord a) => WeightedSeachState a where
--    produce :: a -> [(Int, a)]

-- TODO: цена дуги явно определяется не только текущей вершиной, но и правилами пространства:
