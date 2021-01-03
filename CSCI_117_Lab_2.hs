-- CSci 117, Lab 2:  Functional techniques, iterators/accumulators,
-- and higher-order functions


---- Part 1: Basic structural recursion ----------------

-- 1. Merge sort

-- Deal a list into two (almost) equal-sizes lists by alternating elements
-- For example, deal [1,2,3,4,5,6,7] = ([1,3,5,7], [2,4,6])
deal :: [a] -> ([a],[a])
deal [] = ([], [])
deal [x] = ([x], [])
deal (x:y:zs) = let (xs, ys) = deal zs in (x:xs, y:ys)

-- Now implement merge and mergesort from class, and test with some
-- scrambled lists to make sure your code is correct

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys


ms :: Ord a => [a] -> [a]
ms [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = deal xs
           in merge (mergesort as) (mergesort bs)
           where
               (as, bs) = deal xs


-- 2. A backward list data structure

-- Back Lists: Lists where elements are added to the back ("snoc" == rev "cons")
-- For example, the list [1,2,3] is represented as Snoc (Snoc (Snoc Nil 1) 2) 3
data BList a = Nil | Snoc (BList a) a deriving Show

-- Add an element to the beginning of a BList, like (:) does
cons :: a -> BList a -> BList a
cons x Nil  = Snoc Nil x
cons x (Snoc l y) = Snoc (cons x l) y

-- Convert a usual list into a BList (hint: use cons)
toBList :: [a] -> BList a
toBList [] = Nil
toBList (x:xs) = cons x (toBList xs)

-- Add an element to the end of a usual list
snoc :: [a] -> a -> [a]
snoc [] a = [a]
snoc (y:ys) x = y:snoc ys x


-- Convert a BList into a usual list (hint: use snoc)
fromBList :: BList a -> [a]
fromBList Nil  = []
fromBList (Snoc l y) = snoc (fromBList l) y

-- 3. A tree data structure
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

-- Count number of Empty's in the tree
num_empties :: Tree a -> Int
num_empties Empty = 1
num_empties (Node a t1 t2) = (num_empties t1) + (num_empties t2)

-- Count number of Node's in the tree
num_nodes :: Tree a -> Int
num_nodes Empty = 0
num_nodes (Node a t1 t2) = 1 + (num_nodes t2) + (num_nodes t1)

-- Insert a new node in the leftmost spot in the tree
insert_left :: a -> Tree a -> Tree a
insert_left x Empty = Node x Empty Empty
insert_left x (Node a t1 t2) = Node a (insert_left x t1) t2

-- Insert a new node in the rightmost spot in the tree
insert_right :: a -> Tree a -> Tree a
insert_right x Empty = Node x Empty Empty 
insert_right x (Node a t1 t2) = Node a (insert_right x t2) t1

-- Add up all the node values in a tree of numbers
sum_nodes :: Num a => Tree a -> a
sum_nodes Empty = 0
sum_nodes (Node a t1 t2) = (sum_nodes t1) + a + (sum_nodes t2)

-- Produce a list of the node values in the tree via an inorder traversal
-- Feel free to use concatenation (++)
inorder :: Tree a -> [a]
inorder Empty = []
inorder (Node a t1 t2) = (inorder t1) ++ [a] ++ (inorder t2)

-- 4. A different tree data structure
data Tree2 a = Leaf a | Node2 a (Tree2 a) (Tree2 a) deriving Show

-- Count the number of elements in the tree (leaf or node)
num_elts :: Tree2 a -> Int
num_elts (Leaf a) = 1
num_elts (Node2 a left right) = 1 + num_elts left + num_elts right

-- Add up all the elements in a tree of numbers
sum_nodes2 :: Num a => Tree2 a -> a
sum_nodes2 (Leaf a) = 1
sum_nodes2 (Node2 a left right) = a + sum_nodes2 left + sum_nodes2 right

-- Produce a list of the elements in the tree via an inorder traversal
-- Again, feel free to use concatenation (++)
inorder2 :: Tree2 a -> [a]
inorder2 (Leaf a) = [a]
inorder2 (Node2 a left right) = inorder2 left ++ [a] ++ inorder2 right

-- Convert a Tree2 into an equivalent Tree1 (with the same elements)
conv21 :: Tree2 a -> Tree a
conv21 (Leaf a) = Node a Empty Empty
conv21 (Node2 a left right) = (Node a (conv21 left) (conv21 right))

---- Part 2: Iteration and Accumulators ----------------


-- Both toBList and fromBList from Part 1 Problem 2 are O(n^2) operations.
-- Re-implement them using iterative helper functions with accumulators
-- to make them O(n)
toBList' :: [a] -> BList a
toBList' xs = toBList_it xs Nil where
  toBList_it [] a = a
  toBList_it (x:xs) a = toBList_it xs (Snoc a x)


fromBList' :: BList a -> [a]
fromBList' xs = fromBList_it xs [] where
  fromBList_it Nil a = a
  fromBList_it (Snoc xs x) a  = fromBList_it xs (x:a)



-- Even tree functions that do multiple recursive calls can be rewritten
-- iteratively using lists of trees and an accumulator. For example,
sum_nodes' :: Num a => Tree a -> a
sum_nodes' t = sum_nodes_it [t] 0 where
  sum_nodes_it :: Num a => [Tree a] -> a -> a
  sum_nodes_it [] a = a
  sum_nodes_it (Empty:ts) a = sum_nodes_it ts a
  sum_nodes_it (Node n t1 t2:ts) a = sum_nodes_it (t1:t2:ts) (n+a)

-- Use the same technique to convert num_empties, num_nodes, and sum_nodes2
-- into iterative functions with accumulators

num_empties' :: Tree a -> Int
num_empties' t = num_empties_it [t] 0 where
  num_empties_it :: [Tree a] -> Int-> Int
  num_empties_it [] a = 1
  num_empties_it (Empty:ts) a = num_empties_it ts (a+1)
  num_empties_t (Node n t1 t2:ts) a = num_empties_it (t1:t2:ts) (n+a)

num_nodes' :: Tree a -> Int
num_nodes' t = num_nodes_it [t] 0 where
  num_nodes_it :: [Tree a] -> Int -> Int
  num_nodes_it [] a = a
  num_nodes_it (Empty:ts) a = num_nodes_it ts (a+1)
  num_nodes_it (Node n t1 t2:ts) a = num_nodes_it (t1:t2:ts) (a+1)


sum_nodes2' :: Num a => Tree2 a -> a
sum_nodes2' t = sum_nodes2_it [t] 0 where
sum_nodes2_it :: Num a => [Tree2 a] -> a -> a
sum_nodes2_it [Leaf n] a = a+n
sum_nodes2_it (Leaf n:ts) a = sum_nodes2_it ts (a+n)
sum_nodes2_it (Node2 n t1 t2:ts) a = sum_nodes2_it (t1:t2:ts) (n+a)


-- Use the technique once more to rewrite inorder2 so it avoids doing any
-- concatenations, using only (:).
-- Hint 1: (:) produces lists from back to front, so you should do the same.
-- Hint 2: You may need to get creative with your lists of trees to get the
-- right output.

inorder2' :: Tree2 a -> [a]
inorder2' t = inorder2_it [t] [] where
  --inorder2_it :: Tree2 a -> [a] -> a
  inorder2_it [Leaf n] a = n:a
  inorder2_it (Leaf n:ts) a = inorder2_it ts (n:a)
  inorder2_it ((Node2 n t1 t2):ts) a = inorder2_it (t1:ts)  (n:inorder2_it (t2:ts) a)




---- Part 3: Higher-order functions ----------------

-- The functions map, all, any, filter, dropWhile, takeWhile, and break
-- from the Prelude are all higher-order functions. Reimplement them here
-- as list recursions. break should process each element of the list at
-- most once. All functions should produce the same output as the originals

my_map :: (a -> b) -> [a] -> [b]
my_map_[] = []
my_map f (x:xs) = (f x) : (my_map f xs)

my_all :: (a -> Bool) -> [a] -> Bool
my_all f [] = True
my_all f (x:xs)
  | f x = True
  | otherwise = my_all f xs

my_any :: (a -> Bool) -> [a] -> Bool
my_any_[] = False
my_any f (x:xs)
  | f x = True
  | otherwise = my_any f xs

my_filter :: (a -> Bool) -> [a] -> [a]
my_filter p [] = []
my_filter p (x:xs)
  | p x = x: my_filter p xs
  | otherwise = my_filter p xs


my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile_[] = []
my_dropWhile f (x:xs)
  | f x = my_dropWhile f xs
  | otherwise = x : xs

my_takeWhile :: (a -> Bool) -> [a] -> [a]
my_takeWhile_[] = []
my_takeWhile f (x:xs)
  | f x = x : (x:(my_takeWhile f xs))
  | otherwise = []

my_break :: (a -> Bool) -> [a] -> ([a], [a])
my_break_[] = []
my_break f xs = (my_takeWhile (not . f) xs, my_dropWhile (not . f) xs)

-- Implement the Prelude functions and, or, concat using foldr

my_and :: [Bool] -> Bool
my_and (xs) = foldr (&&) True xs

my_or :: [Bool] -> Bool
my_or (xs) = foldr (||) False xs

my_concat :: [[a]] -> [a]
my_concat (xs) = foldr (++) [] xs

-- Implement the Prelude functions sum, product, reverse using foldl

my_sum :: Num a => [a] -> a
my_sum xs = foldl(+) 0 xs

my_product :: Num a => [a] -> a
my_product xs = foldl (*) 1 xs

my_reverse :: [a] -> [a]
my_reverse xs = foldl (\xs x -> x:xs) [] xs

---- Part 4: Extra Credit ----------------

-- Convert a Tree into an equivalent Tree2, IF POSSIBLE. That is, given t1,
-- return t2 such that conv21 t2 = t1, if it exists. (In math, this is called
-- the "inverse image" of the function conv21.)  Thus, if conv21 t2 = t1, then
-- it should be that conv 12 t1 = Just t2. If there does not exist such a t2,
-- then conv12 t1 = Nothing. Do some examples on paper first so you can get a
-- sense of when this conversion is possible.
conv12 :: Tree a -> Maybe (Tree2 a)
conv12 = undefined


-- Binary Search Trees. Determine, by making only ONE PASS through a tree,
-- whether or not it's a Binary Search Tree, which means that for every
-- Node a t1 t2 in the tree, every element in t1 is strictly less than a and
-- every element in t2 is strictly greater than a. Complete this for both
-- Tree a and Tree2 a.

-- Hint: use a helper function that keeps track of the range of allowable
-- element values as you descend through the tree. For this, use the following
-- extended integers, which add negative and positive infinities to Int:

data ExtInt = NegInf | Fin Int | PosInf deriving Eq

instance Show ExtInt where
  show NegInf     = "-oo"
  show (Fin n) = show n
  show PosInf     = "+oo"

instance Ord ExtInt where
  compare NegInf  NegInf  = EQ
  compare NegInf  n       = LT
  compare (Fin n) (Fin m) = compare n m
  compare (Fin n) PosInf  = LT
  compare PosInf  PosInf  = EQ
  compare _       _       = GT
  -- Note: defining compare automatically defines <, <=, >, >=, ==, /=

bst :: Tree Int -> Bool
bst Empty = True
bst (Node a Empty Empty) = True


bst2 :: Tree2 Int -> Bool
bst2 = undefined
