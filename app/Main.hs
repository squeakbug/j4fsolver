module Main where

import Data.Graph
import Data.List.Split (splitOn)
import System.IO

data AppState = AppState {
    currentGraph :: Graph
} deriving Show

data Event = Init
           | ReadGraph FilePath
           | WriteGraph FilePath
           | OutputGraphviz FilePath
           | Exit
           deriving (Eq, Show)

updateState :: AppState -> Event -> IO AppState
updateState state (ReadGraph filePath) = do
    currentGraph <- readGraph filePath
    return $ AppState { currentGraph }

updateState state (WriteGraph filePath) = do
    writeGraph filePath (currentGraph state)
    return state

updateState state (OutputGraphviz filePath) = do
    outputGraphviz filePath (currentGraph state)
    return state

updateState state Exit = return state

menu :: AppState -> IO ()
menu initState = do
    putStrLn "1. Read graph from file"
    putStrLn "2. Write current graph to file"
    putStrLn "3. Output graph in Graphviz form"
    putStrLn "4. Exit from application"
    putStrLn "Choose an option: "
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Enter filename: "
            filename <- getLine
            newState <- updateState initState (ReadGraph filename)
            graph <- readGraph filename
            putStrLn "Graph loaded."
            menu newState
        "2" -> do
            putStrLn "Enter filename: "
            filename <- getLine
            newState <- updateState initState (WriteGraph filename) 
            putStrLn "Graph written."
            menu newState
        "3" -> do
            putStrLn "Enter filename: "
            filename <- getLine
            newState <- updateState initState (OutputGraphviz filename)
            menu newState
        "4" -> do
            putStrLn "Exiting..."
        _ -> do
            putStrLn "Invalid option. Please try again."
            newState <- updateState initState Exit
            menu newState

emptyGraph :: Graph
emptyGraph = buildG (0, -1) []

main :: IO ()
main = menu AppState { currentGraph = emptyGraph }
