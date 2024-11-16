module LibSolver.Search
    ( Node(..)
    , Problem(..)
    , root
    , path
    , expand
    ) where

-- Узлы можно сопоставлять по некоторой мере в определенной алгебре 
-- (детерменированный и недетерменированный контекст)

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
expand :: (Problem p s a) => p s a -> Node s a -> [Node s a]
expand p node = [ mkNode a s | (a,s) <- successor p (state node) ]
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

    -- | Принимает значение списка состояний и действий для их достижения
    --   Список материализуется лениво
    successor :: p s a -> s -> [(a, s)]

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

---------------------------------------------------------------------------------

-- Рассуждения до чтения AIMA)

-- Хотелось бы обходить граф
-- 1) Без зависимости от его представления (явно задан через ребра, вершины или неявно)
-- 2) Без зависимости от типа вершины

-- Возможные варианты реализации:
-- 1) Стоит реализовать определенный интерфейс для графа
-- 2) Реализовать тип над графом, реализующий некоторый интерфейс

-- С другой стороны, поиск можно вести не только на графе, но и в другой структуре (конечной и бесконечной)
-- Главное предоставить простой интерфейс для тех, кто задает пространство поиска, а не тех, кто пишет алгоритм поиска

---------------------------------------------------------------------------------

-- Некоторым алгоритмам поиска достаточно информации о смежных вершинах:

-- Для операции "поиск в графе" необходим следующий интерфейс от графа
-- 1) По вершине найти список смежных вершин
-- 2) По вершине найти список смежных вершин с весами до этих вершин

-- Для операции "поиск в графе" необходим следующий интерфейс от вершины
-- 1) Проверить, является ли две вершины равными

-- Интерфейс поиска
-- 1) Граф -> исходная вершина -> конечная вершина -> Maybe [Label]

---------------------------------------------------------------------------------

-- Некоторые алгоритмы поиска используют информацию о расстоянии между двумя вершинами:
-- Существует "эффективная" процедура нахождения расстояния между двумя вершинами
-- * Расстояние может быть приближенным
-- * Расстояние может быть получено из накопленной в процессе поиска информации

-- Для операции "поиск в графе" необходим следующий интерфейс от графа
-- 1) По вершине найти список смежных вершин
-- 2) По вершине найти список смежных вершин с весами до этих вершин

-- Для операции "поиск в графе" необходим следующий интерфейс от вершины
-- 1) Проверить, является ли две вершины равными

-- Интерфейс поиска
-- 1) Граф -> исходная вершина -> конечная вершина -> Maybe [Label]

---------------------------------------------------------------------------------

-- Некоторым алгоритмами достаточно информация о порядке вершин:
-- Вершина A ближе к вершине B, чем к вершине C 

-- Для операции "поиск в графе" необходим следующий интерфейс от графа
-- 1) По вершине найти список смежных вершин
-- 2) По вершине найти список смежных вершин с весами до этих вершин

-- Для операции "поиск в графе" необходим следующий интерфейс от вершины
-- 1) Проверить, является ли две вершины равными

-- Интерфейс поиска
-- 1) Граф -> исходная вершина -> конечная вершина -> Maybe [Label]
