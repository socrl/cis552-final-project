module Queue where

data Queue a  =  Queue [a] [a]

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue b f) = Queue (x:b) f

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] [])     = Nothing
dequeue (Queue b  (x:xs)) = Just (x, Queue b xs)
dequeue (Queue b  [])     = dequeue (Queue [] (reverse b))

single :: a -> Queue a
single x = (Queue [x] [])