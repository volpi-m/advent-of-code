{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where

import System.Environment (getArgs)
import Data.List (dropWhileEnd, dropWhile, isPrefixOf, tails, findIndex)
import Data.Char (isSpace, isDigit)

type Color = (Int, String)

type Set = [Color]

type Game = [Set]

findString :: String -> String -> Int
findString search str = case findIndex (isPrefixOf search) (tails str) of
                            Just a -> a
                            Nothing -> -1

takeString :: String -> String -> String
takeString sub str = undefined

strToInt :: String -> Int
strToInt s = read s :: Int

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

cut     :: (Char -> Bool) -> String -> [String]
cut p s =  case dropWhile p s of
                "" -> []
                s' -> w : cut p s''
                    where (w, s'') = break p s'

findColors :: String -> [String]
findColors set = map (last . cut (==' ')) $ cut (==',') set

findNumbers :: String -> [Int]
findNumbers set = map (strToInt . takeWhile isDigit . trim) (cut (==',') set)

generateSets :: [String] -> Game
generateSets x = foldr (\ x -> (++) [zip (findNumbers x) (findColors x)]) [] x

generateGames :: [[String]] -> [Game]
generateGames xs = map generateSets xs

getNumber :: String -> Set -> Int
getNumber _ [] = -1
getNumber c (x:xs)
    | snd x == c = fst x
    | otherwise = getNumber c xs

isSetPossible :: Set -> Bool
isSetPossible set = redC <= 12 && greenC <= 13 && blueC <= 14
    where
        redC = getNumber "red" set
        greenC = getNumber "green" set
        blueC = getNumber "blue" set

isAllTrue :: [Bool] -> Bool
isAllTrue [] = True
isAllTrue (x:xs) = x && isAllTrue xs

isGamePossible :: Game -> Bool
isGamePossible xs = isAllTrue $ map isSetPossible xs

total :: Int -> [Bool] -> Int
total _ [] = 0
total n (x:xs)
    | x = n + total (n + 1) xs
    | otherwise = total (n + 1) xs

star1 :: String -> Int
star1 content = total 1 $ map isGamePossible $ generateGames [cut (== ';') (last x) | x <- map (cut (== ':')) $ lines content]

getPower :: Game -> Int
getPower g = maxRed * maxGreen * maxBlue
    where
        maxRed = maximum $ map (getNumber "red") g
        maxGreen = maximum $ map (getNumber "green") g
        maxBlue = maximum $ map (getNumber "blue") g

star2 :: [Game] -> Int
star2 [] = 0
star2 (x:xs) = getPower x + star2 xs

main :: IO ()
main = do
    args <- getArgs
    content <- readFile $ head args
    --print $ star1 content
    print $ star2 $ generateGames [cut (== ';') (last x) | x <- map (cut (== ':')) $ lines content]
