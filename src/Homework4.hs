fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . (map go) . filter even 
  where go x =  x - 2



main :: IO ()
main = do
  putStrLn $ (show . fun1) [1, 3, 4, 32, 13, 32, 103]
  putStrLn $ (show . fun1') [1, 3, 4, 32, 13, 32]

