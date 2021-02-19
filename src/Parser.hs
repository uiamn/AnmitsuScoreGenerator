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


parse :: FilePath -> IO Score
parse path = do
    rawFile <- readFile path -- ファイルを読み出す
    let lines = map removeCR $ splitOn "\n" rawFile -- 行毎に区切る
    let mainDataLines = filter (\x -> ("#" `isPrefixOf` x) && isNumber (x !! 1)) lines -- メインデータが格納されている行

    return Score {
        header = parseHeader lines,
        objects = concatMap lineToObjects mainDataLines,
        wavDef = parseDefinition "WAV" lines
    }


parseHeader :: [String] -> Header
parseHeader lines = Header {
    title = drop 7 $ head $ filter (isPrefixOf "#TITLE") lines,
    bpm = read (drop 5 $ head $ filter (isPrefixOf "#BPM") lines) :: Double,
    total = read (drop 7 $ head $ filter (isPrefixOf "#TOTAL") lines) :: Double,
    judgeLevel = toEnum (read (drop 6 $ head $ filter (isPrefixOf "#RANK") lines) :: Int) :: JudgeLevel
}

parseDefinition :: String -> [String]  -> [Definition]
parseDefinition prefix lines = map ((\x -> Definition (head x) (last x)) . splitOn " " . drop (length prefix + 1)) $ filter (isPrefixOf ("#" ++ prefix)) lines


lineToObjects :: String -> [Object]
lineToObjects line = do
    let bar = read (take 3 $ tail line) :: Int
    let channel = hexToDec $ take 2 $ drop 4 line
    let objects = stepSplit 2 $ drop 7 line
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

removeCR :: String -> String
removeCR str = do
    case length str of
        0 -> str
        _ -> case last str of
            '\r' -> take (length str - 1) str
            _ -> str
