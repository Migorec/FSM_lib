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
    
 