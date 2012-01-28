module Main where

import FSM_a
import Database.HDBC
import Database.HDBC.Sqlite3


run :: IO ()
run = do conn <- connectSqlite3 "test.db"
         run_A conn "fsm_table" "msg_table" "timer_table" 1
         disconnect conn