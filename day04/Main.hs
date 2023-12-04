module Main where

import System.Environment (getArgs)

cut     :: (Char -> Bool) -> String -> [String]
cut p s =  case dropWhile p s of
                "" -> []
                s' -> w : cut p s''
                    where (w, s'') = break p s'

yeet :: [String] -> String -> Bool
yeet win n = n `elem` win

cutNumbers :: [String] -> [[String]]
cutNumbers card = map (cut (==' ')) card

findWinningNumber :: [String] -> [String] -> [Bool]
findWinningNumber win own = map (yeet win) own

findWin :: [[String]] -> [Bool]
findWin numLists = findWinningNumber (head numLists) (last numLists)

countPoints :: Int -> [Bool] -> Int
countPoints base [] = div base 2
countPoints base (x:xs)
    | x = countPoints (base * 2) xs
    | otherwise = countPoints base xs

star1 :: String -> Int
star1 content = sum $ map (countPoints 1 . findWin . cutNumbers . cut (=='|') . last . cut (==':')) (lines content)

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    print $ star1 content
