{-# LANGUAGE DatatypeContexts #-}

module LibSolver.Search where

-- | Узел в дереве поиска.
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
root p = Node 
    { state=s
    , parent=Nothing
    , action=Nothing
    , cost=0 
    , depth=0 
    , value=valueP p s
    } where s = initial p

path :: Node s a -> [Node s a]
path n = case parent n of
    Nothing -> [n]
    Just n' -> n : path n'

-- |Возвращает список узлов, непосредственно достижимых из данного узла в контексте указанной проблемы.
--  Обновляет depth у дочерних узлов
expand :: (Problem p s a) => p s a -> Node s a -> [Node s a]
expand p node = [ mkNode a (result p (state node) a) | a <- actions p (state node) ]
    where
        mkNode a s = Node s (Just node) (Just a) (c a s) (1 + depth node) v
        c          = costP p (cost node) (state node)
        v          = valueP p (state node)

---------------------------------------------------------------------------------

type Cost = Double

-- | Задача разрешения или задача оптимизации:
--  s - тип состояния
--  a - тип действия агента
-- 
--  Минимально требуется реализовать метод 'initial' и 'successor', и одну из функций:
--  'goal' - если возможно сравнить состояние с конечным состоянием 
--  'goalTest' - если цель конечное состояние определяется по предикату
class Eq s => Problem p s a where
    -- | Начальное состояние
    initial :: p s a -> s

    -- | Список доступных из состояния действий
    actions :: p s a -> s -> [a]

    -- | Результат применения действия в данном состоянии
    result :: p s a -> s -> a -> s

    -- | Если у задачи есть только одно конечное состояние, этот метод должен вернуть его
    goal :: p s a -> s
    goal = undefined

    -- | Возвращает true, если состояние является целью. 
    --   Метод по умолчанию сравнивает состояние с состоянием, указанным в реализации 'goal'. 
    --   Вы можете переопределить этот метод, если для определения конечной используется определенный предикат
    goalTest :: p s a -> s -> Bool
    goalTest p s = s == goal p

    -- | Возвращает стоимость пути решения, которое исходит из первого состояния и завершается вторым состоянием. 
    --   Если проблема такова, что путь не имеет значения, то функция будет учитывать только второе состояние. 
    --   В реализации по умолчанию стоимость каждого шага на пути равна 1.
    costP :: p s a -> Cost -> s -> a -> s -> Cost
    costP _ c _ _ _ = c + 1

    -- | Возможно, вы захотите указать эвристическую функцию для этой задачи. 
    --   Реализация по-умолчанию всегда возвращает ноль.
    -- 
    --   Это аналог 'h' функции из AIMA. Пример использования можно найти в реализации A* алгоритма
    heuristic :: p s a -> Node s a -> Cost
    heuristic _ = const 0

    -- | В задачах оптимизации (а не разрешения) каждое состояние имеет свое значение. 
    --   Hill-climbing и связанные с ним алгоритмы пытаются максимизировать это значение. 
    --   Реализация по умолчанию всегда возвращает ноль.
    valueP :: p s a -> s -> Double
    valueP _ = const 0