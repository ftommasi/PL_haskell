--Set Module
module Set  where
import qualified Data.List as L
  --Set is a sorted list with no repeated elements
data Set a = Set [a] 
  deriving (Show,Eq,Ord)
--take a list, sort it, remove duplicates and cast it to set
list2set :: Ord a => [a] -> Set a
list2set = Set . L.nub . L.sort

--cast a set as list
set2list :: Set a -> [a]
set2list (Set xs) = xs

--get the union of two sets
unionS :: (Ord a) => Set a -> Set a -> Set a
unionS (Set xs) (Set ys) = Set $  (merge xs ys) where 
  merge [] ys = ys 
  merge xs [] = xs 
  merge (x:xs) (y:ys)  
    | x<y = x:merge xs (y:ys) 
    | x>y = y:merge (x:xs) ys 
    | otherwise = x:merge xs ys

--cast single element as set
singS :: a -> Set a
singS x = Set $ (x:[])

--short hand syntactic sugar for an empy list/empty set
emptyS :: Set a
emptyS = Set $ []

--Set wrapper for list.head
setHead :: Set a -> a
setHead xs = head $ set2list xs

--Set wrapper for list.tail
setTail :: Set a -> [a]
setTail xs = tail $ set2list xs

--cast a list directly as a Set with no sorting or ordering 
listAsSet :: [a] -> Set a
listAsSet xs = Set xs

--add a single element to a set
addToS :: (Ord a) => a -> Set a -> Set a
addToS e xs | xs == emptyS = singS $ e
addToS e xs | e > (setHead xs) = Set $ ((setHead xs) : (set2list $  addToS e $ Set $ (setTail  xs)))
            | e == (setHead xs) = xs 
            | otherwise = Set (e :(set2list  xs))

--get the subset of intersecting elements between two sets (AND)
intersectS :: (Ord a) => Set a -> Set a -> Set a
intersectS xs ys 
                 | xs == emptyS = emptyS
                 | ys == emptyS = emptyS
                 | (setHead xs) == (setHead ys) = addToS (setHead xs) (intersectS  (listAsSet (setTail  xs)) (listAsSet (setTail ys)))  
                 | (setHead xs) > (setHead ys) = (intersectS xs (listAsSet (setTail ys)))
                 | (setHead xs) < (setHead ys) = (intersectS (listAsSet (setTail xs))  ys )

--get the subset of different elements between two sets (OR)
diffS :: (Ord a) => Set a -> Set a -> Set a
diffS xs ys | xs == emptyS && ys == emptyS = emptyS
            | xs == emptyS && ys /= emptyS = ys
            | xs /= emptyS && ys == emptyS = xs
            | (setHead xs) == (setHead ys) = diffS (listAsSet (setTail xs)) (listAsSet (setTail ys))
            | (setHead xs) < (setHead ys) = addToS (setHead xs) (diffS (listAsSet (setTail xs)) ys)
            | (setHead xs) > (setHead ys) = addToS (setHead ys) (diffS  xs (listAsSet (setTail ys)) )

--retrun whether or not xs is a subset of ys
subseteq :: (Ord a) => Set a -> Set a -> Bool
subseteq xs ys 
               | xs == emptyS = True
               | ys == emptyS = False
               | (setHead xs) == (setHead ys) = subseteq (listAsSet (setTail xs)) (listAsSet (setTail ys))
               | otherwise  = False 


--extra credit
--helper keeps track of how far down the lists we are with i
subsequenceHelper :: Eq a => Int -> [a] -> [a] -> [Int]
subsequenceHelper _ _ [] = []
subsequenceHelper _ [] _ = []
subsequenceHelper i (x:xs) (y:ys) | x == y = (i:(subsequenceHelper (i+1) xs ys))
                                  | otherwise = (subsequenceHelper (i+1) (x:xs) ys)

--return the indeces on which the elements of subset xs happen in ys
subsequence :: Eq a => [a] -> [a] -> [Int]
subsequence _ [] = []
subsequence [] _ = []
subsequence xs ys = subsequenceHelper 0 xs ys
-- /Set Module


