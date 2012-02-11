{-# LANGUAGE MultiParamTypeClasses #-}

module FSM_a (run_A, A_Messages(..), A_Answer(..), A_States(..)) where

import RSOI.FSMlib
import Database.HDBC
import Control.Concurrent.RWLock

timeout_b = 1
timeout_c = 2
retries_b = 8
retries_c = 4

data A_States = Init | 
                Wait | 
                Wait_B | 
                Wait_C | 
                Done | 
                Cancel deriving (Eq,Show,Read)
                
data A_Messages = Start |
                  Timeout_B |
                  Timeout_C |
                  Ack_B |
                  Ack_C |
                  Nack deriving (Eq,Show,Read,Ord)

newtype A_Answer = A{answer::String} deriving (Eq,Read,Show)
                  
data A_Data = A_Data { retries_B :: Int,
                       retries_C :: Int,
                       reply_B :: Maybe A_Answer,
                       reply_C :: Maybe A_Answer } deriving (Eq,Read,Show)
                       
                       
instance FSM A_States A_Data A_Messages A_Answer where

    init = (Init,A_Data{retries_B = 0, retries_C = 0, reply_B = Nothing, reply_C = Nothing})
    
    timeout Wait = [(Timeout_B, timeout_b), (Timeout_C, timeout_c)]
    timeout Wait_B = [(Timeout_B, timeout_b)]
    timeout Wait_C = [(Timeout_C, timeout_c)]
    timeout _ = []
    
    state Init Start dat _ = Just (Wait, dat)
    
    state Wait Ack_B dat ans = Just (Wait_C, dat{reply_B = Just ans})
    state Wait_B Ack_B dat ans = Just (Done, dat{reply_B = Just ans})
    state Wait Ack_C dat ans = Just (Wait_B, dat{reply_C = Just ans})
    state Wait_C Ack_C dat ans = Just (Done, dat{reply_C = Just ans})
    
    state Wait Nack dat _ = Just (Cancel, dat)
    state Wait_B Nack dat _ = Just (Cancel, dat)
    state Wait_C Nack dat _ = Just (Cancel, dat)
    
    state Wait Timeout_B dat _ | retries_B dat < retries_b = Just (Wait,dat)
                               | otherwise = Just (Cancel,dat)
    
    state Wait_B Timeout_B dat _ | retries_B dat < retries_b = Just (Wait_B,dat)
                                 | otherwise = Just (Cancel,dat)
                                  
    state Wait Timeout_C dat _ | retries_C dat < retries_c = Just (Wait,dat)
                               | otherwise = Just (Cancel,dat)
    
    state Wait_C Timeout_C dat _ | retries_C dat < retries_c = Just (Wait_C,dat)
                                 | otherwise = Just (Cancel,dat)
                                 
    state _ _ _ _ = Nothing
    
    
    action Wait Start dat _ = do putStrLn "Start"
                                 return dat{retries_B = 1, retries_C = 1}
    action Wait Timeout_B dat _ = do let n = retries_B dat
                                     return dat{retries_B = n +1}
    action Wait_B Timeout_B dat _ = do let n = retries_B dat
                                       return dat{retries_B = n +1}
    action Wait Timeout_C dat _ = do let n = retries_C dat
                                     return dat{retries_C = n +1}
    action Wait_C Timeout_C dat _ = do let n = retries_C dat
                                       return dat{retries_C = n +1}
    
    action Cancel Timeout_B dat _ = do  putStrLn "Timeout B. Cancel."
                                        return dat
    action Cancel Timeout_C dat _ = do putStrLn "Timeout C. Cancel."
                                       return dat
    action Cancel Nack dat _ = do putStrLn "Nack. Cancel."
                                  return dat
    
    action Done _ dat _ = do putStrLn "Done."
                             return dat
    
    action _ _ dat _ = return dat
    
run_A :: IConnection c => c -> String -> String -> String -> Int -> RWLock -> IO (A_States, A_Data, A_Messages, A_Answer )
run_A = runFSM     