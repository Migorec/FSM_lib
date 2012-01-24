{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module RSOI.FSMlib(FSM(..),
                   runFSM) where

import Database.HDBC
import Control.Monad (when, liftM)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromJust) 

-- | Typeclass for finite state machine Класс коннечного автомата
--
-- Parameters: Параметры:
--
-- * FSM state Состояние автомата (s)
--
-- * request state (d)
--
-- * messages (m)
--
-- * answer (data of the message) (a)


class (Eq s, Show s, Read s,
       Eq m, Show m, Read m,
       Show d, Read d,
       Show a, Read a) => FSM s d m a  | s -> d, s -> m, s -> a where
    -- | State-transition function. Функция перехода автомата. 
    state :: s -- ^ previous FSM state предыдущее состояние автомата
          -> m -- ^ message recieved полученное сообщение
          -> d -- ^ previuos request state предыдущее состояние заявки
          -> a -- ^ answer from remote system (data of the message)
          -> (s, d) -- ^ new FSM and rewuest state новое состояние автомата и заявки
    
    -- | Function defining timers which should be started when entering new state. Функция определяющая таймеры, которые необходимо запустить при переходе в новое состояние
    -- Часть функции выхода
    timeout :: s -- ^ new FSM state новое состояние автомата
            -> [(m,Int)] -- ^ list of pairs (message,time) список пар (таймер, время)
            
    -- | Output function. Функция выхода автомата
    action :: s -- ^ new FSM state новое состояние автомата
           -> m -- ^ message that changed state сообщение, по которому был выполнен переход
           -> d -- ^ request state состояние заявки
           -> a -- ^ answer from remote system (data of the message)
           -> IO d -- ^ new request state + side effects (such as sending message to another system) новое состояние заявки + побочные эффекты (отправка сообщений другим системам, к примеру)

-- | main function
runFSM :: (IConnection c,
           FSM s d m a )  => c  -- ^ coonection to databse 
                          -> String -- ^ state table name
                          -> String -- ^ request table name
                          -> String -- ^ timer table name
                          -> Int -- ^ period in seconds
                          -> IO (s,d,m,a)
runFSM conn stName rtName ttName pTime =
    do tables <- getTables conn
       when (not (stName `elem` tables)) $
            do run conn ("CREATE TABLE " ++ stName ++ "(id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, " ++
                                                      "state VARCHAR (25) NOT NULL, " ++
                                                      "data VARCHAR (1000) NOT NULL)") []
               return () 
       when (not (rtName `elem` tables)) $
            do run conn ("CREATE TABLE " ++ rtName ++ "(id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, " ++
                                                      "fsm_id INTEGER NOT NULL, " ++
                                                      "msg VARCHAR (25) NOT NULL, " ++
                                                      "data VARCHAR (1000), " ++
                                                      "FOREIGN KEY  (fsm_id) REFERENCES " ++ stName ++ "(id)") []
               return ()
       when (not (ttName `elem` tables)) $
            do run conn ("CREATE TABLE " ++ ttName ++ "(id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT, " ++
                                                      "fsm_id INTEGER NOT NULL, " ++
                                                      "msg VARCHAR (25) NOT NULL, " ++
                                                      "time INTEGER NOT NULL, " ++
                                                      "FOREIGN KEY (fsm_id) REFERENCES " ++ stName ++ "(id)") []
               return ()
            
       loopFSM conn stName rtName ttName pTime     
       
loopFSM :: (IConnection c,
            FSM s d m a) => c -> String -> String -> String -> Int -> IO (s,d,m,a)
loopFSM conn stName rtName ttName pTime = do
    checkTimers conn rtName ttName
    res <- checkMessages conn stName rtName
    threadDelay $ pTime * 1000000
    if True
        then loopFSM conn stName rtName ttName pTime
        else return (fromJust res)
    
    
    
checkMessages :: (IConnection c,
                  FSM s d m a) => c -> String -> String -> IO (Maybe (s,d,m,a))
checkMessages conn stName rtName = do
    message <- head `liftM` quickQuery' conn ("SELECT * FROM " ++ rtName ++ " ORDER BY id LIMIT 1") []
    if message==[]
       then return Nothing
       else do let [mid_s,fid_s,msg_s,mdat_s] = message
               [fid_s,st_s,fdat_s]<- head `liftM`  quickQuery' conn ("SELECT * FROM " ++ stName ++ "WHERE id=?") [fid_s]
               let st = read $ fromSql st_s
                   fdat = read $ fromSql fdat_s
                   msg = read $ fromSql msg_s
                   mdat = read $ fromSql mdat_s
                   (new_st, i_dat) = state st msg fdat mdat
                   timers = timeout new_st 
               new_dat <- action new_st msg i_dat mdat
    
               run conn ("DELETE FROM " ++ rtName ++ " WHERE id=?") [mid_s]
               run conn ("UPDATE" ++ stName ++ "SET state=? data=? WHERE id = ?") [fid_s,toSql $ show new_st, toSql $ show new_dat]
               commit conn
               if True
                  then checkMessages conn stName rtName
                  else return (Just (new_st,new_dat,msg,mdat))
               
       
checkTimers ::(IConnection c) => c -> String -> String -> IO ()
checkTimers conn rtName ttName =
    do timeouts <- quickQuery' conn ("SELECT id, fsm_id, msg FROM " ++ ttName ++ " WHERE time < datetime('now') ORDER BY time") []
       ins <- prepare conn ("INSERT INTO " ++ rtName ++ " (fsm_id, msg) VALUES (?,?)")
       del <- prepare conn ("DELETE FROM " ++ ttName ++ " WHERE id=?")
       executeMany ins (map tail timeouts)
       executeMany del (map (\t -> [head t]) timeouts)  
       commit conn
