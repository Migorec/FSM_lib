{-# LANGUAGE MultiParamTypeClasses #-}

module FSM_a where

import RSOI.FSMlib

timeout_b = 10
timeout_c = 20
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