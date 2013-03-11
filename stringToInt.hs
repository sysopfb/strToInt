import Data.Char (digitToInt, isDigit)

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = loop acc' xs
	where
	acc' = acc * 10 + digitToInt x

strToInt :: String -> Int
strToInt xs = loop 0 xs

loop' acc [] = acc
loop' acc (x:xs) 
	| isDigit x = loop' acc' xs
	| otherwise = loop' acc xs
	where
	acc' = acc * 10 + digitToInt x

strToIntAll :: String -> Int
strToIntAll xs = loop' 0 xs

strToIntAll' :: String -> Integer
strToIntAll' xs = foldl (\acc x -> (acc * 10) + (fromIntegral ( digitToInt x))) 0 (filter isDigit xs)
