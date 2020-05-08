-- 1202 Program Alarm
import Data.List.Split (endBy)
import Data.List (splitAt)


removeNonInts :: String -> String
removeNonInts = filter (/= '\n') -- note '\n' is parsed as single char because \ is special


numStrToIntList :: String -> [Int]
numStrToIntList s = map (\ c -> read c :: Int) $ endBy "," s


replaceNth :: Int -> Int -> [Int] -> [Int]
replaceNth _ _ [] = []
replaceNth num idx xs
    | length xs == 1 = xs
    | otherwise = 
        let (l, _:r) = splitAt idx xs in
            l ++ [num] ++ r


-- this is working
parseIntCode :: [Int] -> Int -> [Int]
parseIntCode xs ocIdx
    | xs !! ocIdx == 99 = xs
    | otherwise = 
        let oc = xs !! ocIdx
            p1elem = xs !! (xs !! (ocIdx + 1))
            p2elem = xs !! (xs !! (ocIdx + 2))
            outputIdx = xs !! (ocIdx + 3)
            output = if oc == 1
                     then p1elem + p2elem 
                     else p1elem * p2elem
            nextOcIdx = ocIdx + 4
        in
            parseIntCode (replaceNth output outputIdx xs) nextOcIdx
    



main :: IO()
main = do
    rawInp <- readFile "./input.txt" 
    let inp = numStrToIntList . removeNonInts $ rawInp
    let stagedInp = replaceNth 2 2 $ replaceNth 12 1 inp

    let answer = head $ parseIntCode stagedInp 0 
    print answer
