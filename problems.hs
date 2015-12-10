-- 99 Haskell Problems: https://wiki.haskell.org/99_questions
import Data.List

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
compress = map head . group

-- 9) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x == (head xs)
              then (x:(head $ pack xs)):(pack $ tail xs)
              else [x]:(pack xs)

-- 10) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = foldr compress' [] . map encoded
  where encoded x = (1,x)
        compress' t [] = [t]
        compress' t ((n,y):ys)
          | snd t == y = compress' (n+1,y) ys
          | otherwise  = t:(compress' (n,y) ys)
