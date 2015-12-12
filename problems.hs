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
pack = foldr compress [] . map wrap
  where wrap = flip (:) []
        compress x [] = [x]
        compress x (y:ys)
          | head x == head y = compress (head x : y) ys
          | otherwise = x:(compress y ys)

-- 10) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode = foldr compress [] . map encoded
  where encoded x = (1,x)
        compress t [] = [t]
        compress t ((n,y):ys)
          | snd t == y = compress (n+1,y) ys
          | otherwise  = t:(compress (n,y) ys)

-- 11) Modified run-length encoding.
-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data Numerable a = Multiple Int a | Single a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Numerable a]
encodeModified =  foldr compress [] . map encoded
  where encoded x = Single x
        compress x [] = [x]
        compress (Multiple n x) (Multiple m y:ys) = (Multiple n x):(compress (Multiple m y) ys)
        compress (Single x) (Single y:ys)
          | x == y    = compress (Multiple 2 y) ys
          | otherwise = (Single x):(compress (Single y) ys)
        compress (Single x) (Multiple n y:ys)
          | x == y    = compress (Multiple (n+1) y) ys
          | otherwise = (Single x):(compress (Multiple n y) ys)
        compress (Multiple n x) (Single y:ys)
          | x == y    = compress (Multiple (n+1) y) ys
          | otherwise = (Multiple n x):(compress (Single y) ys)

-- 12) Decode a run-length encoded list.
-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: (Eq a) => [Numerable a] -> [a]
decodeModified = concat . map decoded
  where decoded (Single x) = [x]
        decoded (Multiple n x) = replicate n x
