import Debug.Trace
import Data.Char

createList :: Int -> [[Char]]
createList 0 = []
createList size = [] : createList (size - 1)

insertInStackList :: [[Char]] -> [Char] -> Int -> [[Char]]
insertInStackList (x:xs) elem 0 = elem : xs
insertInStackList (x:xs) elem idx = x : insertInStackList xs elem (idx - 1)

populateWithLine :: String -> [[Char]] -> Int -> Int -> [[Char]]
populateWithLine [] l _ _ = l
populateWithLine line l acc listIdx =
    if head line == '[' then populateWithLine (tail line) newL newAcc newIdx
    else populateWithLine (tail line) l newAcc newIdx
    where
        newAcc = if acc == 3 then 0 else acc + 1
        newIdx = if acc == 3 then listIdx + 1 else listIdx
        newL = insertInStackList l ((l !! listIdx) ++ [line !! 1]) listIdx

populateStack :: [String] -> [[Char]] -> [[Char]]
populateStack (x:xs) l
    | x == "" = l
    | otherwise = populateStack xs newStacks
    where
        newStacks = populateWithLine x l 0 0

cut :: (Char -> Bool) -> String -> [String]
cut c s = case dropWhile c s of
            "" -> []
            s' -> w : cut c s''
                where (w, s'') = break c s'

isInt :: String -> Bool
isInt [] = True
isInt (x:xs) = isNumber x && isInt xs

processInstructions :: [String] -> [Int]
processInstructions [] = []
processInstructions (x:xs)
    | isInt x = (read x :: Int) : processInstructions xs
    | otherwise = processInstructions xs

createInstructionList :: [String] -> Bool -> [[Int]]
createInstructionList [] b = []
createInstructionList (x:xs) b
    | x == "" = createInstructionList xs True
    | otherwise = if b then instruction : createInstructionList xs True else createInstructionList xs False
    where
        instruction = processInstructions (cut (==' ') x)

moveOnce :: [[Char]] -> Int -> Int -> [[Char]]
moveOnce l from to = insertInStackList (insertInStackList l (tail $ l !! idxFrom) idxFrom) newStack idxTo
    where
        idxFrom = from - 1
        idxTo = to - 1
        el = head $ l !! idxFrom
        newStack = el : (l !! idxTo)

moveMultiple :: Int -> Int -> Int -> [[Char]] -> [[Char]]
moveMultiple 0 _ _ l = l
moveMultiple nbTimes from to l = moveMultiple (nbTimes - 1) from to (moveOnce l from to)

executeInstructions :: [[Char]] -> [[Int]] -> [[Char]]
executeInstructions l [] = l
executeInstructions l (x:xs) = executeInstructions nextStacks xs
    where
        nextStacks = moveMultiple (head x) (x !! 1) (x !! 2) l

moveAllAtOnce :: Int -> Int -> Int -> [[Char]] -> [[Char]]
moveAllAtOnce nbMove from to l = insertInStackList (insertInStackList l newToStack idxTo) newFromStack idxFrom
    where
        idxFrom = from - 1
        idxTo = to - 1
        payload = take nbMove (l !! idxFrom)
        newToStack = payload ++ (l !! idxTo)
        newFromStack = drop nbMove (l !! idxFrom)

executeInstructions9000 :: [[Char]] -> [[Int]] -> [[Char]]
executeInstructions9000 l [] = l
executeInstructions9000 l (x:xs) = executeInstructions9000 nextStacks xs
    where
        nextStacks = moveAllAtOnce (head x) (x !! 1) (x !! 2) l

takeFirsts :: [[Char]] -> String
takeFirsts [] = []
takeFirsts (x:xs) = (head x) : takeFirsts xs

main :: IO ()
main = do
    content <- readFile "input"
    let
        tabLines = lines content
        emptyList = createList $ (length (head tabLines) + 1) `div` 4
        stacks = populateStack tabLines emptyList
        instructionList = createInstructionList tabLines False in do
            print $ takeFirsts $ executeInstructions stacks instructionList -- star 1
            print $ takeFirsts $ executeInstructions9000 stacks instructionList -- star 2
