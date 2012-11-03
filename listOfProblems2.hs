import Data.List

data ListItem a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]

decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified xs = foldl (\acc e -> case e of Single x -> acc ++ [x]; Multiple n x -> acc ++ replicate n x) [] xs

encodeDirect :: (Eq a) => [a] -> [ListItem a]
encodeDirect xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]

duplicate :: (Ord a) => [a] -> [a]
duplicate = concatMap (replicate 2) 

replicate' :: (Ord a) =>  Int -> [a] -> [a]
replicate' n xs = concatMap (replicate n) xs

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = take (n-1) xs ++ dropEvery (drop n xs) n

split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs) 

slice :: (Ord a) => [a] -> Int -> Int -> [a]
slice xs i k = drop (i-1) $ take k xs

rotate :: (Ord a) => [a] -> Int -> [a]
rotate xs n = drop n xs ++ take n xs

removeAt :: (Ord a) => Int -> [a] -> [a]
removeAt n xs = take (n-1) xs ++ drop n xs

main :: IO ()
main = do
 -- print $ encodeModified "mississippi"

--  print $ decodeModified [Single 'b', Multiple 2 'o', Multiple 2 'k', Multiple 2 'e', Single 'p', Single 'e', Single 'r']

--  print $ encodeDirect "peppermint"

--  print $ duplicate "versicherungskarte"

--  print $ replicate' 3 "abc"

  --print $ dropEvery "watermelon" 3

--  print $ split "automobile" 4

--  print $ slice "thisismysliceofpie" 7 13

  --print $ rotate [1,4,9,16,25,36,49,64,81,100] 4
  
 -- print $ removeAt 7 "tidalwave"
