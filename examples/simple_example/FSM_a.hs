module FSM_a where


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