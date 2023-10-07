{-
-- EPITECH PROJECT, 2023
-- Wolfram
-- File description:
-- Main
-}

module Main (main) where
import System.Environment
import System.Exit (exitWith, ExitCode(..))
import Arguments
import Rules

main :: IO ()
main = getArgs >>= \args ->
    case getOpts defaultConf args of
        Nothing -> putStrLn "./wolfram --rule [number] --[opt] [number]"
            >> exitWith (ExitFailure 84)
        Just conf' -> let rules = generateRuleTable (rule conf') in
            generate rules (start conf') (formatLines (line conf'))
            (createSides (window conf'))
            (move conf') (firstLine (window conf') (move conf'))
                >> exitWith (ExitSuccess)
