--Set Module
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

--syntactic sugar for a single element
singleton :: Char -> String
singleton x = (x:"")

--returns how many of the first character happen in a row
inArow ::  Char -> String ->Int
inArow _ xs | (length xs) < 1 = 0
inArow c (x:xs) 
  | x == c  = 1 + (inArow c (xs))
  | otherwise = 0

--pop i characters from the front of the string (x:xs)
getNext :: Int -> String -> String
getNext _ "" = ""
getNext i (x:xs) | i > 0 = getNext (i-1) xs
  | otherwise = (singleton x) ++ getNext (i-1) xs 
--Run Length Encoding
rle :: String -> String
rle xs | (length xs) < 1 = ""
rle (x:xs) = let numchar = (inArow x (x:xs)) in
  (show numchar) ++ (singleton x) ++ " " ++ (rle (getNext numchar (x:xs)))

--get substring until first instance of given character
takeUntil :: Char -> String -> String
takeUntil _ "" = ""
takeUntil c (x:xs) 
  | c == x = ""
  | otherwise = (singleton x) ++ takeUntil c xs

--get length of takeUntil
charsUntil :: Char -> String -> Int
charsUntil c xs = (length (takeUntil c xs))

--Run Length Decoding
rInverse :: String -> String
rInverse "" = ""
rInverse xs = let substring = (takeUntil ' ' xs  ) in 
  (foldr 
    (++) 
    "" 
    (replicate (read (take ((length substring) - 1) substring) :: Int ) 
      (take 1 (reverse substring)))) 
    ++ (rInverse (getNext ((charsUntil ' ' xs) + 1) xs))
