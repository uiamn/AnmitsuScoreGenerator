module Types where

import Data.Ratio

data Definition = Definition String String

data JudgeLevel = VeryHard | Hard | Normal | Easy deriving (Show, Enum)

data Object = Object {
    bar_ :: Int,
    position :: Rational,
    channel :: Int,
    index :: String
}

data Header = Header {
    title :: String,
    -- artist :: String
    bpm :: Double,
    total :: Double,
    judgeLevel :: JudgeLevel
} deriving (Show)

data Score = Score {
    header :: Header,
    objects :: [Object],
    wavDef :: [Definition]
}
