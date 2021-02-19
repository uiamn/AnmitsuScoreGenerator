module Types where

import Data.Ratio

data WavDef = WavDef Int String

data Object = Object {
    bar_ :: Int,
    position :: Rational,
    channel :: Int,
    index :: String
}

data Header = Header {
    title :: String,
    artist :: String
}

data Score = Score {
    header :: Header,
    objects :: [Object],
    wavDef :: [WavDef]
}
