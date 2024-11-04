{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module LibSolver.Agent where

import System.IO.Unsafe

-- |Любой физический объект в окружении
class Show o => Object o where
    isAlive :: o -> Bool

-----------------------------------------------------------------------------

-- |Интеллектуальный агент
class Object (agent p a) => Agent agent p a where
    -- |Ответное действие агента на воздействие среды
    program :: agent p a -> p -> a

newtype TraceAgent agent p a = TraceAgent { getAgent :: agent p a } deriving (Show)

instance Object (agent p a) => Object (TraceAgent agent p a) where
    isAlive (TraceAgent agent) = isAlive agent

instance (Agent agent p a, Show p, Show a) => Agent (TraceAgent agent) p a where
    program (TraceAgent agent) p = unsafePerformIO $ do
        let a = program agent p
        putStrLn $
            show agent ++ " perceives " ++ show p ++ " and does " ++ show a
        return a

-----------------------------------------------------------------------------

-- |Окружение, содержащее агенты и объекты. 
--  Изменяется вследствие действий (actions) агентов.
--  Оказывает воздействие (percept) на агентов
class (Object o, Agent agent p a) => Environment e o agent p a where
    -- |Список объектов в окружении
    objects :: e o agent p a -> [o]

    -- |Список агентов в окружении
    agents :: e o agent p a -> [agent p a]

    -- |Воздействие окружения, которое зафиксировал агент
    percept :: e o agent p a -> agent p a -> p

    -- |Измените окружение, чтобы отразить действие агента.
    execAction :: e o agent p a -> agent p a -> a -> e o agent p a

    -- |Выполнение закончено? 
    --  По умолчанию, выполнение завершается, когда нельзя найти живого агента.
    isDone :: e o agent p a -> Bool
    isDone env = not $ any isAlive $ agents env
