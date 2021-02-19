module Parser where

import Types

import Data.Char
import Data.List
import Data.List.Split
import Data.Ratio

import Numeric

hexToDec :: String -> Int
hexToDec str = fst $ head $ readHex str


isNote :: Object -> Bool
isNote obj
    | channel obj <= hexToDec "10" = False
    | channel obj == hexToDec "17" = False
    | channel obj >= hexToDec "1A" = False
    | otherwise = True

countNotes :: [Object] -> Int
countNotes objects = length $ filter isNote objects


parse :: FilePath -> IO ()
parse path = do
    mainData <- preMainDataParse path

    print $ countNotes mainData


preMainDataParse :: FilePath -> IO [Object]
preMainDataParse path = do
    row_file <- readFile path
    let f = removeCR row_file
    let mainDataLines = toMainDataLines f

    return $ concatMap lineToObjects mainDataLines


lineToObjects :: String -> [Object]
lineToObjects str = do
    let bar = read (take 3 $ tail str) :: Int
    let channel = hexToDec $ take 2 $ drop 4 str
    let objects = stepSplit 2 $ drop 7 str
    let denom = fromIntegral(length objects) :: Integer

    map (\(position, id_) -> Object {
        bar_ = bar,
        position = position % denom,
        channel = channel,
        index = id_
    }) $ filter (\(_, id_) -> id_ /= "00") $ zip [0..] objects


stepSplit :: Int -> String -> [String]
stepSplit step str
    | length str <= step = [str]
    | otherwise = take step str : stepSplit step (drop step str)


toMainDataLines :: String -> [String]
toMainDataLines str = filter isMainDataLine $ map removeCR $ splitOn "\n" str


removeCR :: String -> String
removeCR str = do
    case length str of
        0 -> str
        _ -> case last str of
            '\r' -> take (length str - 1) str
            _ -> str


isMainDataLine :: String -> Bool
isMainDataLine str = ("#" `isPrefixOf` str) && isNumber (str !! 1)
