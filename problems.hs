-- 99 Haskell Problems: https://wiki.haskell.org/99_questions

-- 1) Find the last element of a list.
myLast :: [a] -> a
myLast = head . reverse

-- 2) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast = last . take 2 . reverse

-- 3) Find the K'th element of a list. The first element in the list is number 1
elementAt :: [a] -> Int -> a
elementAt xs n = last $ take n xs

-- 4) Find the number of elements of a list.
myLength :: [a] -> Int
myLength = foldl (\x _ -> x + 1) 0

-- 5) Reverse a list.
myReverse :: [a] -> [a]
myReverse = foldl (\ys x -> x:ys) []

-- 6) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

-- 7) Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten' :: NestedList a -> [a]
flatten' (Elem x) = [x]
flatten' (List x) = concatMap flatten' x

-- 8) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress (x:xs@(y:_))
  | x == y = compress xs
  | otherwise = x:(compress xs)
compress xs = xs
