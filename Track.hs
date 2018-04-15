module Track where

type TaskList = [TaskRecord]
type TaskRecord = (TaskId,Person,Status,TimeStamp)
type TaskId = Int
type Person = String
data Status = Start | Done | Pause | Resume
    deriving (Eq, Ord, Show)
type TimeStamp = (Int,Int,Int,Int,Int)
type Message = String


start :: Person -> TaskId -> TimeStamp -> TaskList -> Either Message TaskList 
start p id ts tl | taskExist id tl = Left "Task already started" 
                 | otherwise       = Right $ (id,p,Start,ts):tl
    where
    taskExist id tl = not $ null $ filter (\(i,_,_,_) -> id==i) tl 

newTaskList :: TaskList
newTaskList = []
