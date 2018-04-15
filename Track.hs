module Track where

type TaskList = [TaskRecord]
type TaskRecord = (TaskId,Person,Status,TimeStamp)
type TaskId = Int
type Person = String
data Status = Start | InProgress | Waiting | Done
    deriving (Eq, Ord, Show)
type TimeStamp = (Int,Int,Int,Int,Int)
type Message = String


start :: Person -> TaskId -> TimeStamp -> TaskList -> Either Message TaskList 
start p id ts tl | not (null (filter (\(id',_,_,_) -> id'== id) tl)) = Left "Task already started" 
                 | otherwise                                         = Right $ (id,p,Start,ts):tl

newTaskList :: TaskList
newTaskList = []
