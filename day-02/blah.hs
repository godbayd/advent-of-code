rec :: Int -> [Int] -> [Int]
rec n (x:xs)
    | n < 5 = x : rec (n + 1) xs
    | otherwise = []
    
-- 1 < (ceiling $ 12 / 5 :: Int) will evaluate
-- but line 10 wont ??? 
rec' :: Int -> [Int] -> [Int]
rec' n (x:xs)
    | n < (ceiling (length (x:xs)) / 4) = x : rec (n + 4) xs
    | otherwise = []
    

main :: IO()
main = 
    print "hello"
