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
