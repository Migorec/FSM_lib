{-# LANGUAGE MultiParamTypeClasses #-}

module RSOI.FSMlib where

import Database.HDBC
import Control.Monad (when)

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
--
-- * timers (t)

class (Eq s, Show s, Read s,
       Eq m, Show m, Read m,
       Eq t, Show t, Read t,
       Show d, Read d,
       Show a, Read a) => FSM s d m a t where
    -- | State-transition function. Функция перехода автомата. 
    state :: s -- ^ previous FSM state предыдущее состояние автомата
          -> m -- ^ message recieved полученное сообщение
          -> d -- ^ previuos request state предыдущее состояние заявки
          -> a -- ^ answer from remote system (data of the message)
          -> (s, d) -- ^ new FSM and rewuest state новое состояние автомата и заявки
    
    -- | Function defining timers which should be started when entering new state. Функция определяющая таймеры, которые необходимо запустить при переходе в новое состояние
    -- Часть функции выхода
    timeout :: s -- ^ new FSM state новое состояние автомата
            -> [(t,Int)] -- ^ list of pairs (timer,time) список пар (таймер, время)
            
    -- | Output function. Функция выхода автомата
    action :: s -- ^ new FSM state новое состояние автомата
           -> m -- ^ message that changed state сообщение, по которому был выполнен переход
           -> d -- ^ request state состояние заявки
           -> a -- ^ answer from remote system (data of the message)
           -> IO d -- ^ new request state + side effects (such as sending message to another system) новое состояние заявки + побочные эффекты (отправка сообщений другим системам, к примеру)

-- | main function
runFSM :: (IConnection c) => c  -- ^ coonection to databse 
                          -> String -- ^ state table name
                          -> String -- ^ request table name
                          -> String -- ^ timer table name
                          -> Int -- ^ period in seconds
                          -> IO ()
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
       
loopFSM :: (IConnection c) => c -> String -> String -> String -> Int -> IO ()
loopFSM conn stName rtName ttName pTime =
    checkTimers conn rtName ttName
       
checkTimers ::(IConnection c) => c -> String -> String -> IO ()
checkTimers conn rtName ttName =
    do timeouts <- quickQuery' conn ("SELECT id, fsm_id, msg FROM " ++ ttName ++ " WHERE time < datetime('now') ORDER BY time") []
       ins <- prepare conn ("INSERT INTO " ++ rtName ++ " (fsm_id, msg) VALUES (?,?)")
       del <- prepare conn ("DELETE FROM " ++ ttName ++ " WHERE id=?")
       executeMany ins (map tail timeouts)
       executeMany del (map (\t -> [head t]) timeouts)  
       commit conn
