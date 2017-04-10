--Fausto Tommasi


--problem 1
-- problem1-part a helper
addToAll :: Int -> [Int] -> [Int]
addToAll  _ [] = []
addToAll x xs = (x + (head xs) : addToAll x (tail xs))

--part a
addFirstA :: [Int] -> [Int]
addFirstA [] = []
addFirstA (x:xs) =  ( x+x : (addToAll x xs))


--part b
addFirstB :: [Int] -> [Int]
addFirstB [] = []
addFirstB (x:xs) = (x+x :map (+ x) xs)


--problem 2
commaSeparate :: [String] -> String
commaSeparate xs 
                 | (length xs) > 1  = (head xs) ++ ", " ++ commaSeparate (tail xs)
                 | (length xs) == 1 = (head xs)
                 | otherwise        = ""

--problem 3
deleteAll :: (Eq a) => a -> ([a] ->[a])
deleteAll _ [] = []
deleteAll a (x:xs) 
                   | x == a = deleteAll a xs
                   | otherwise = (x : (deleteAll a xs))

--problem 4

--problem4-helper
deleteFirst :: (Eq a) => a ->  [a] -> [a]
deleteFirst a (x:xs) 
                     | x == a  = xs
                     | otherwise = (x : (deleteFirst a xs))

deleteSecond :: (Eq a) => a -> ([a] -> [a])
deleteSecond _ [] = []
deleteSecond a (x:xs) 
                      | x == a = (x : (deleteFirst a xs))
                      | otherwise = deleteSecond a xs

--problem 5
associated :: (Eq a) => a -> [(a,b)] -> [b]
associated _ [] = []
associated x (y:ys) | x == (fst y) = ((snd y) : (associated x ys))
                    | otherwise = associated x ys


--problem 6 (Extra Credit attempt)
isSorted :: (Ord a) => (a -> a-> Bool) -> [a] -> Bool
isSorted _ [] = True
isSorted _ xs | (length xs) < 2 = True
isSorted p (x1:x2:xs) | (p x1 x2) = isSorted p xs
                      | otherwise = False

--Bubble sort algorithm that takes a predicate as input for custom sorting
mySortF :: (Ord a) => (a -> a-> Bool) -> [a] -> [a]
mySortF_ [] = []
mySortF _ xs | (length xs) < 2 = xs
mySortF p (x1:x2:xs) | (isSorted p (x1:(x2:xs))) = (x1:(x2:xs))
                    | (p x1 x2) = (mySortF p (x1:(mySortF p (x2:xs))))
                    | otherwise = (mySortF p (x2:(mySortF p (x1:xs)))) 

--shorthand quickness for non decreasing sort
mySort :: (Ord a) => [a] -> [a]
mySort xs = mySort (<) xs


