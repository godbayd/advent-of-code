import Data.List.Split (endBy)

calc :: String -> Int
calc x = floor $ (read x :: Float) / 3 - 2

main :: IO()
main = do
    x <- readFile "./input.txt"
    let inpList = filter (/= "") $ endBy "\n" x
    print $ sum $ fmap calc inpList 
