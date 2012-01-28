module Main where

import FSM_a
import Database.HDBC
import Database.HDBC.Sqlite3


main :: IO ()
main = do conn <- connectSqlite3 "test.db"
          run_A conn "fsm_table" "msg_table" "timer_table" 1
          disconnect conn
         
reply_B :: Int -> IO ()
reply_B fid= conn <- connectSqlite3 "test.db"
             run conn "INSERT INTO msg_table (fsm_id,msg,data) VALUES (?,?,?)" [toSql fid, toSql $ show Ack_B, toSql $ show A{answer = "B answer"}]
             commit conn
             disconnect conn
             
reply_C :: Int -> IO ()
reply_C fid= conn <- connectSqlite3 "test.db"
             run conn "INSERT INTO msg_table (fsm_id,msg,data) VALUES (?,?,?)" [toSql fid, toSql $ show Ack_C, toSql $ show A{answer = "C answer"}]
             commit conn
             disconnect conn             
             