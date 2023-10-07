{-
-- EPITECH PROJECT, 2023
-- Wolfram
-- File description:
-- Arguments
-}

module Arguments (Conf(..), defaultConf, getOpts) where
import Data.Char

data Conf = Conf {
    rule   :: Maybe Int,
    start  :: Int,
    line   :: Maybe Int,
    window :: Int,
    move   :: Maybe Int
} deriving (Show)

defaultConf :: Conf
defaultConf = Conf {
    rule   = Nothing,
    start  = 0,
    line   = Nothing,
    window = 80,
    move   = Nothing
}

isInt :: String -> Bool
isInt [] = False
isInt ('-' : xs) = isInt xs
isInt list
    | False `elem` (map (isDigit) (list)) = False
    | otherwise = True

isNeg :: String -> Bool
isNeg ('-' : _) = True
isNeg _ = False

getOpts :: Conf -> [String] -> Maybe Conf
getOpts Conf{rule = Nothing} [] = Nothing
getOpts conf [] = Just conf
getOpts _ (_ : arg : _)
    | isInt arg == False = Nothing
getOpts conf ("--move":arg:args) = getOpts (conf {move = Just (read arg)}) args
getOpts _ (_:arg:_)
    | isNeg arg == True = Nothing
getOpts conf ("--start":arg:args) = getOpts (conf {start = read arg}) args
getOpts onf ("--lines":arg:args) = getOpts (onf {line = Just (read arg)}) args
getOpts conf ("--window":arg:args) = getOpts (conf {window = read arg}) args
getOpts _ (_:arg:_)
    | (read arg :: Int) > 255 = Nothing
getOpts conf ("--rule":arg:args) = getOpts (conf {rule = Just (read arg)}) args
getOpts _ _ = Nothing
