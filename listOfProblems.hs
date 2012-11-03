import Data.List

last' :: (Ord a) =>  [a] -> a
last' = head . reverse

butLast :: (Ord a) => [a] -> a
butLast = head . tail . reverse

elementAt :: (Ord a) => [a] -> Int -> a
elementAt xs i = xs !! (i-1)
-- since the function must return a value from a list that is 1 indexed then integer must be subtracted by one.

length' :: (Ord a) => [a] -> Int
length' (x:xs) = 1 + length' xs

reverse' :: (Ord a) => [a] -> [a]
reverse' (x:xs) = reverse xs ++ [x]

palindrome :: (Eq a) => [a] -> Bool
palindrome xs = xs == reverse xs

flatten :: (Ord a) => [[a]] -> [a]
flatten = concat 

compress :: (Eq a) => [a] -> [a]
compress xs = map head $ group xs

pack :: (Eq a) => [a] -> [[a]]
pack = group 

encode :: (Ord a) => [a] -> [(Int,a)]
encode xs = zip (map length $ pack xs) (map head $ pack xs) 

main :: IO ()
main = do
 -- print $ last' list
  --  where list = [1,2,3,4,5]

  --print $ butLast list
    --where list = [1,2,3,4,5]

 -- print $ elementAt list 7
  --  where list = "sicherlich"

--  print $ length' list
--    where list = [345, 72, 1733, 349912, 251, 92, 14] 

 -- print $ reverse list
  --  where list = ["go", "to", "where", "I", "want", "to", "go"]

 -- print $ palindrome "ardvark"

--  print $ flatten [[11,10,9,8],[1,2,3],[7,0],[4,5,6]]

--  print $ compress [455, 378, 22, 62, 34, 455, 455, 62, 378, 34, 378, 378]

--  print $ pack "aaapraammm"

--  print $ encode "bookkeeper"
