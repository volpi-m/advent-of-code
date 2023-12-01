module Main where

import System.Environment (getArgs)
import Data.Char (isDigit, ord)
import Data.List (isPrefixOf)

elemIndex :: Eq a => a -> [a] -> Int
elemIndex n l = elemIndexIntern n l 0
    where
        elemIndexIntern :: Eq a => a -> [a] -> Int -> Int
        elemIndexIntern _ [] _ = -1
        elemIndexIntern n (x:xs) idx
            | n == x = idx
            | otherwise = elemIndexIntern n xs (idx + 1)

findFirstDigit :: String -> Char
findFirstDigit line = line !! elemIndex True (map isDigit line)

analyseLine :: String -> Int
analyseLine l = (ord (findFirstDigit l) - 48) * 10 + ord (findFirstDigit $ reverse l) - 48

transformLine :: String -> String
transformLine [] = []
transformLine l@(x:xs)
    | "one" `isPrefixOf` l = "1" ++ transformLine xs --(drop 2 xs)
    | "two" `isPrefixOf` l = "2" ++ transformLine xs -- (drop 2 xs)
    | "three" `isPrefixOf` l = "3" ++ transformLine xs -- (drop 4 xs)
    | "four" `isPrefixOf` l = "4" ++ transformLine xs -- (drop 3 xs)
    | "five" `isPrefixOf` l = "5" ++ transformLine xs -- (drop 3 xs)
    | "six" `isPrefixOf` l = "6" ++ transformLine xs -- (drop 2 xs)
    | "seven" `isPrefixOf` l = "7" ++ transformLine xs -- (drop 4 xs)
    | "eight" `isPrefixOf` l = "8" ++ transformLine xs -- (drop 4 xs)
    | "nine" `isPrefixOf` l = "9" ++ transformLine xs -- (drop 3 xs)
    | otherwise = x : transformLine xs

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args)
    print $ sum $ map (analyseLine . transformLine) (lines content)
