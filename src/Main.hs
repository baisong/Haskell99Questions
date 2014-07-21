----------------------------------------------------------------------------------------------------
-- My solutions to Haskell 99 Questions
-- @see http://www.haskell.org/haskellwiki/99_questions/1_to_10
----------------------------------------------------------------------------------------------------

import Data.List

-- #1. Gets the last item from a list.
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

-- #2. Gets the second from the last item from a list.
myPenultimate :: [a] -> a
myPenultimate [] = error "empty list!"
myPenultimate [x] = error "Gotta have more than 1 item!"
myPenultimate [x, y] = x
myPenultimate (_:xs) = myPenultimate xs

-- ... Other solutions
myPenultimate' = last . init
myPenultimate'' x = reverse x !! 1
myPenultimate''' = head . tail . reverse

-- #3. Gets the Kth item from a list.
eAt :: [a] -> Int -> a
eAt x y = x !! (y -1)

-- ... Other solutions
elementAt'' :: [a] -> Int -> a
elementAt'' (x:_) 1  = x
elementAt'' (_:xs) i = elementAt'' xs (i - 1)
elementAt'' _ _      = error "Index out of bounds"

-- #4 Find length of list
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- ... using an accumulator
myLength' :: [a] -> Int
myLength' list = myLength_acc list 0
	where
		myLength_acc [] n = n
		myLength_acc (_:xs) n = myLength_acc xs (n + 1)

-- #5 Reverse a list
myReverse :: [a] -> [a]
myReverse []     = []
myReverse [x]    = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- ... formal function definition
myReverse'          :: [a] -> [a]
myReverse'          =  foldl (flip (:)) []

-- similar, but more readable
myReverse'' :: [a] -> [a]
myReverse'' list = reverse''' list []
  where
    reverse''' [] reversed     = reversed
    reverse''' (x:xs) reversed = reverse''' xs (x:reversed)

-- #6 Find whether palindrome or no
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = (myReverse xs) == xs

-- ... other solutions
isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' xs  = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

-- Allow for nested lists like (1 (2, (3, 4), 5))
data NestedList a = Elem a | List [NestedList a]

-- #7 Flatten a nested list.
flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)
flatten (List [])     = []

-- ... Using a where block to hold an xs parameter.
flatten2 :: NestedList a -> [a]
flatten2 a = flt' a []
  where flt' (Elem x)      xs = x:xs
        flt' (List (x:ls)) xs = flt' x (flt' (List ls) xs)
        flt' (List [])     xs = xs

-- ... Terse solution using concatMap
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten' x

-- #8 Compress
--
-- \> compress "aabcccccccccccccccc"
-- "abc"
--
-- If a list contains repeated elements
-- they should be replaced with a single copy of the element.
-- The order of the elements should not be changed.
compress :: (Eq a) => [a] -> [a]
compress []       = []
compress [x]      = [x]
compress (x:xs)   = if x == head xs then compress xs else [x] ++ compress xs

-- ...super terse
-- uses Data.List.group
compress' :: Eq a => [a] -> [a]
compress' = map head . group

-- ...super terse no type definition needed!
compress'' xs = map head $ group xs

-- ...An alternative solution is
compress''' (x:ys@(y:_))
    | x == y    = compress''' ys
    | otherwise = x : compress''' ys
compress''' ys = ys

-- ...Another possibility using foldr
compress'''' :: (Eq a) => [a] -> [a]
compress'''' = foldr skipDups []
    where skipDups x [] = [x]
          skipDups x acc
                | x == head acc = acc
                | otherwise = x : acc

-- ...A very simple approach:
compress''''' []     = []
compress''''' (x:xs) = x : (compress''''' $ dropWhile (== x) xs)

-- #9 Pack
--
-- \> pack "aaaabccaadeeee"
-- ["aaaa","b","cc","aa","d","eeee"]
--

-- Simple solution
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

-- Using "@" (read "as") syntactic sugar and a recursive where clause after pattern guards.
-- http://stackoverflow.com/questions/1153465/what-does-the-symbol-mean-in-reference-to-lists-in-haskell
pack' [] = []
pack' [x] = [[x]]
pack' (x:xs)
    | x == head  h_p_xs = (x : h_p_xs) : t_p_hs
    | otherwise         = [x] : p_xs
    where p_xs@(h_p_xs:t_p_hs) = pack' xs


-- #10
--
-- \> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (myLength x, head x)) (pack xs)
  
-- Run a test.
main = putStrLn $ show (encode "aaaabccaadeeee")
