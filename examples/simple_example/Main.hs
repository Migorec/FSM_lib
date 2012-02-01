module Main where

import FSM_a
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Concurrent
import Directory
import IO

main :: IO ()
main = do bracket (connectSqlite3 "test.db")
                  (\conn -> putStrLn "qqq" >> disconnect conn)
                  (\conn -> run_A conn "fsm_table" "msg_table" "timer_table" 1 >> return ())
                 
start :: Int -> IO ()
start fid = do conn <- connectSqlite3 "test.db"
               run conn "INSERT INTO msg_table (fsm_id,msg) VALUES (?,?)" [toSql fid, toSql $ show Start]
               commit conn
               disconnect conn
          
reply_B :: Int -> IO ()
reply_B fid= do conn <- connectSqlite3 "test.db"
                run conn "INSERT INTO msg_table (fsm_id,msg,data) VALUES (?,?,?)" [toSql fid, toSql $ show Ack_B, toSql $ show A{answer = "B answer"}]
                commit conn
                disconnect conn
             
reply_C :: Int -> IO ()
reply_C fid = do conn <- connectSqlite3 "test.db"
                 run conn "INSERT INTO msg_table (fsm_id,msg,data) VALUES (?,?,?)" [toSql fid, toSql $ show Ack_C, toSql $ show A{answer = "C answer"}]
                 commit conn
                 disconnect conn
             
nack :: Int -> IO ()
nack fid = do conn <- connectSqlite3 "test.db"
              run conn "INSERT INTO msg_table (fsm_id,msg) VALUES (?,?)" [toSql fid, toSql $ show Nack]
              commit conn
              disconnect conn

test :: IO ()
test = do conn <- connectSqlite3 "test.db"
          thread <- forkIO ( run_A conn "fsm_table" "msg_table" "timer_table" 1 >> return () )
          threadDelay $ 1 * 1000000
          start 1
          start 2
          start 3
          start 4
          reply_B 1
          reply_C 1
          start 5
          nack 2
          reply_B 2
          threadDelay $ 2 * 1000000
          reply_C 3
          reply_B 5
          reply_C 5
          threadDelay $ 20 * 1000000
          killThread thread
          disconnect conn
          removeFile "test.db"