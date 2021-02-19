module GenerateVideo where

import Types
import Parser
import Graphics.Gloss

import Numeric

data ObjectBasedTime = ObjectBasedTime {
    second :: Double,
    channel_ :: Int,
    index_ :: String
}

visibleSec = 280.0 / 600.0 :: Double
laneOffset = 30
laneWidth = 30 :: Int
noteHeight = 10
videoHeight = 480 :: Int
laneXPositions = map (\x -> 30*x + laneOffset) [0..8]
judgeLinePosition = 40
objGrid = fromIntegral (videoHeight - judgeLinePosition) / visibleSec


isNote :: ObjectBasedTime -> Bool
isNote obj
    | channel_ obj <= hexToDec "10" = False
    | channel_ obj == hexToDec "17" = False
    | channel_ obj >= hexToDec "1A" = False
    | otherwise = True

objToLane :: ObjectBasedTime -> Int
objToLane obj = case showHex (channel_ obj) "" of
    "16" -> 0
    "11" -> 1
    "12" -> 2
    "13" -> 3
    "14" -> 4
    "15" -> 5
    "18" -> 6
    "19" -> 7
    _ -> -1

laneToColor :: Int -> Color
laneToColor lane
    | lane == 0 = red
    | even lane = blue
    | otherwise = white


generateVideo :: Score -> IO()
generateVideo score = do
    let timeBasedObjects = objectToTimeBased (bpm (header score)) (objects score)

    let scene = generateScene timeBasedObjects 1510

    let window = InWindow "Hello World" (640, videoHeight) (0, 0)

    display window black (translate (-320) (-240) scene)



drawLane :: [Picture]
drawLane = map (\x -> color white $ line [(x, 0), (x, fromIntegral videoHeight)]) laneXPositions


generateScene :: [ObjectBasedTime] -> Int -> Picture
generateScene objects flame = do
    let time = fromIntegral flame / 60.0
    let visibleNotes = filter (\obj -> (second obj >= time) && (second obj < time + visibleSec) && isNote obj) objects

    let lane = drawLane
    let judgeLine = color red $ line [
            (laneOffset, fromIntegral judgeLinePosition),
            (laneOffset + fromIntegral (laneWidth*8), fromIntegral judgeLinePosition)
            ]

    Pictures $ map (drawNoteObject time) visibleNotes ++ (judgeLine : lane)


drawNoteObject :: Double -> ObjectBasedTime -> Picture
drawNoteObject baseTime obj = do
    let restTime = second obj - baseTime
    let y = realToFrac (restTime * objGrid + fromIntegral judgeLinePosition)
    let lane = objToLane obj
    let noteColor = laneToColor lane

    let laneLeft = fromIntegral lane * 30.0 + laneOffset + fromIntegral laneWidth / 2

    -- color noteColor $ polygon [(laneLeft, y), (laneLeft + fromIntegral laneWidth, y), (laneLeft + fromIntegral laneWidth, y+noteHeight), (laneLeft, y+noteHeight)]

    color noteColor $ translate laneLeft y $ rectangleSolid (fromIntegral laneWidth) noteHeight


objectToTimeBased :: Double -> [Object] -> [ObjectBasedTime]
objectToTimeBased bpm objects = do
    map (\obj -> ObjectBasedTime {
        second = (240 :: Double) / bpm * (fromIntegral (bar obj) + fromRational (position obj)),
        channel_ = channel obj,
        index_ = index obj
    }) objects
