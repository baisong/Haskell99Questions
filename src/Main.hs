----------------------------------------------------------------------------------------------------
-- My solutions to Haskell 99 Questions
-- @see http://www.haskell.org/haskellwiki/99_questions/1_to_10
----------------------------------------------------------------------------------------------------

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

-- Run a test.
main = putStrLn $ show $ isPalindrome "AMANAMA"

