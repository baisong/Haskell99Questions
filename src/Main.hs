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

-- Run a test.
main = putStrLn [eAt "12345" 4]
