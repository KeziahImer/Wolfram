{-
-- EPITECH PROJECT, 2023
-- Wolfram
-- File description:
-- Rules
-}

module Rules (generateRuleTable, generate, firstLine, createSides, formatLines)
    where
import Data.Bits

generateRuleTable :: Maybe Int -> [Bool]
generateRuleTable Nothing = []
generateRuleTable (Just n) = map (\i -> testBit n i) [0..7]

createSides :: Int -> (Int, Int)
createSides size = (0, size)

formatLines :: Maybe Int -> Int
formatLines Nothing = -1    
formatLines (Just line) = line

applyRule :: Int -> [Bool] -> String
applyRule index rules
    | rules !! index == True = "*"
    | otherwise = " "

toInt :: String -> Int
toInt "   " = 0
toInt "  *" = 1
toInt " * " = 2
toInt " **" = 3
toInt "*  " = 4
toInt "* *" = 5
toInt "** " = 6
toInt "***" = 7
toInt _ = -1

nextLine :: [Bool] -> String -> Int -> String
nextLine _ str index
    | index >= length str - 1 = ""
nextLine rules str index =
    applyRule (toInt (concat (map pure [str !! (index - 1), str !! index,
    str !! (index + 1)]))) rules
    ++ nextLine rules str (index + 1)

printLine :: String -> (Int, Int) -> IO ()
printLine str (left, right) = putStrLn (take (right + left) (drop (-left) str))

generate :: [Bool] -> Int -> Int -> (Int, Int) -> Maybe Int -> String -> IO ()
generate _ _ 0 _ _ _ = putStr ""
generate rules 0 line (left, right) move str = printLine str (left, right)
    >> generate rules 0 (line - 1) (left - 1, right + 1) move
    (nextLine rules (concat ["  ", str, "  "]) 1)
generate rules start line (left, right) move str 
    = generate rules (start - 1) line (left - 1, right + 1) move
    (nextLine rules (concat ["  ", str, "  "]) 1)

firstLine :: Int -> Maybe Int -> String
firstLine size Nothing = replicate (size `div` 2) ' ' ++ "*" ++
        if size `mod` 2 /= 0
            then replicate (size `div` 2) ' '
            else replicate (size `div` 2 - 1) ' '
firstLine size (Just move) = replicate (size `div` 2 + move) ' ' ++ "*" ++
        if size `mod` 2 /= 0
            then replicate (size `div` 2 - move) ' '
            else replicate (size `div` 2 - 1 - move) ' '
