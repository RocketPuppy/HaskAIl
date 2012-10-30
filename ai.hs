{-# LANGUAGE MultiParamTypeClasses #-}
module PupBot_AI (receiveInput) where

import Data.Convertible.Base
import Database.HDBC
import Database.HDBC.Sqlite3

newtype Word = Word String deriving Show
instance Convertible Word SqlValue where
    safeConvert = Right . SqlString . unWord
instance Convertible SqlValue Word where
    safeConvert (SqlString s) = Right $ Word s
    safeConvert _ = Left $
        ConvertError
            { convSourceValue = "unknown"
            , convSourceType  = "unknown"
            , convDestType    = "Word"
            , convErrorMessage= "Unable to convert unknown SqlType to Word"
            }
unWord (Word x) = x
newtype Node = Node Word deriving Show
instance Convertible Node SqlValue where
    safeConvert = Right . SqlString . unWord . unNode
instance Convertible SqlValue Node where
    safeConvert (SqlString s) = Right $ Node $ Word s
    safeConvert _ = Left $
        ConvertError
            { convSourceValue = "unknown"
            , convSourceType  = "unknown"
            , convDestType    = "Node"
            , convErrorMessage= "Unable to convert unknown SqlType to Node"
            }
unNode (Node x) = x
newtype Edge = Edge (Node, Node) deriving Show
unEdge (Edge x) = x

-- Receive Sentence
-- Given a sentence output a response
receiveInput :: String -> String
receiveInput = joinSentence . splitSentence

-- Split Sentence
splitSentence :: String -> [Word]
splitSentence = map Word . words

-- Join Sentence
joinSentence :: [Word] -> String
joinSentence = unwords . map unWord

-- Database functions

-- connect and make sure foreign keys are on
connect :: IO Connection
connect = do
    conn <- connectSqlite3 "digraph"
    run conn "PRAGMA foreign_keys = ON" []
    return conn

-- Add new words to DB
addWords :: [Word] -> IO ()
addWords ws =
    do  conn <- connect
        query <- prepare conn "INSERT INTO node_values (name) VALUES(?)"
        executeMany query [[toSql w] | w <- ws]
        let str = foldl1 ((++) . (flip (++)) " OR ") ["name=?" | w <- ws]
        query'' <- prepare conn $ "SELECT id FROM node_values WHERE " ++ str
        execute query'' (map toSql ws)
        ids <- fetchAllRows query''
        query' <- prepare conn "INSERT INTO nodes (node_value) VALUES(?)"
        executeMany query' ids
        commit conn
        disconnect conn

-- Make the edges out of words
makeEdges :: [Word] -> [Edge]
makeEdges ws = [Edge (Node x, Node y) | (x:y:[]) <- combos]
    where combos = tail $ scanl1 ((++) . return . last) $ map return ws

-- Add new edges to DB
addEdges :: [Edge] -> IO ()
addEdges es =
    do  conn <- connect
        let whereClause = foldl1 ((++) . (flip (++)) " OR ") ["n1.name=? AND n2.name=?" | e <- es]
        let subQuery = "SELECT n1.id, n2.id FROM (nodes LEFT JOIN node_values ON nodes.node_value=node_values.id) AS n1 CROSS JOIN (nodes LEFT JOIN node_values ON nodes.node_value=node_values.id) AS n2 WHERE " ++ whereClause
        query' <- prepare conn subQuery
        execute query' $ concat [[toSql n1, toSql n2] | (Edge (n1, n2)) <- es]
        ids <- fetchAllRows query'
        query <- prepare conn $ "INSERT OR IGNORE INTO edges (n1, n2) VALUES(?, ?)"
        executeMany query ids
        commit conn
        disconnect conn


-- Increment duplicated edges
incEdges :: [Edge] -> IO ()
incEdges = undefined

-- Find shortest path between TOP and BOTTOM that includes all words in received sentence
findShortestPath :: [Word] -> [Word]
findShortestPath = undefined
