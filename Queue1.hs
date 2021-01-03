module Queue (Queue, mtq, ismt, addq, remq) where

---- Interface ----------------
mtq  :: Queue a                  -- empty queue
ismt :: Queue a -> Bool          -- is the queue empty?
addq :: a -> Queue a -> Queue a  -- add element to front of queue
remq :: Queue a -> (a, Queue a)  -- remove element from back of queue;
                                 --   produces error on empty

---- Implementation -----------
data Queue a = Queue2 [a]

mtq = Queue2 []
ismt (Queue2 []) = True
ismt (Queue2 _) = False
add x (Queue2 []) = Queue2 [x]
addq x (Queue2 xs) = Queue2 (xs ++ [x])
remq (Queue2 []) = error "Can't remove from empty"
remq (Queue2 (xs)) = (head xs, Queue2 $ tail xs)
