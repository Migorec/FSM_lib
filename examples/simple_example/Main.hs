module Main where

import FSM_a
import Database.HDBC
import Database.HDBC.Sqlite3
import Control.Concurrent
import Control.Concurrent.RWLock
import Control.Monad(liftM)
import Directory
import IO

{-
main :: IO ()
main = do bracket (connectSqlite3 "test.db")
                  (\conn -> putStrLn "qqq" >> disconnect conn)
                  (\conn -> run_A conn "fsm_table" "msg_table" "timer_table" 1 >> return ())
-}
               
start :: IConnection c => c -> Int -> IO ()
start conn fid = do run conn "INSERT INTO msg_table (fsm_id,msg) VALUES (?,?)" [toSql fid, toSql $ show Start]
                    commit conn
                
reply_B :: IConnection c => c -> Int -> IO ()
reply_B conn fid = do run conn "INSERT INTO msg_table (fsm_id,msg,data) VALUES (?,?,?)" [toSql fid, toSql $ show Ack_B, toSql $ show A{answer = "B answer"}]
                      commit conn

reply_C :: IConnection c => c -> Int -> IO ()                 
reply_C conn fid = do run conn "INSERT INTO msg_table (fsm_id,msg,data) VALUES (?,?,?)" [toSql fid, toSql $ show Ack_C, toSql $ show A{answer = "C answer"}]
                      commit conn
                 
nack :: IConnection c => c -> Int -> IO ()              
nack conn fid = do run conn "INSERT INTO msg_table (fsm_id,msg) VALUES (?,?)" [toSql fid, toSql $ show Nack]
                   commit conn

test :: IO Bool
test = bracket (do conn <- connectSqlite3 "test.db" 
                   conn' <- clone conn
                   rwl <- newRWLockIO
                   thread <- forkIO ( run_A conn' "fsm_table" "msg_table" "timer_table" 1 rwl >> return () )
                   return (conn,conn',thread,rwl)
               )
               (\(conn,conn',thread,rwl) -> do killThread thread
                                               disconnect conn'
                                               disconnect conn
                                               removeFile "test.db"                                          
               )
               (\(conn,conn',thread,rwl) -> do threadDelay $ 1 * 1000000
                                               withWriteLock rwl $ start conn 1
                                               withWriteLock rwl $ start conn 2
                                               withWriteLock rwl $ start conn 3
                                               withWriteLock rwl $ start conn 4
                                               withWriteLock rwl $ reply_B conn 1
                                               withWriteLock rwl $ reply_C conn 1
                                               withWriteLock rwl $ start conn 5
                                               withWriteLock rwl $ nack conn 2
                                               withWriteLock rwl $ reply_B conn 2
                                               threadDelay $ 2 * 1000000
                                               withWriteLock rwl $ reply_C conn 3
                                               withWriteLock rwl $ reply_B conn 5
                                               withWriteLock rwl $ reply_C conn 5
                                               threadDelay $ 20 * 1000000
                                               res <-  withReadLock rwl $ map (read . fromSql . head ) `liftM` quickQuery' conn "SELECT state FROM fsm_table" []
                                               return (res == [Done,Cancel,Cancel,Cancel,Done])
               )

          