module Types where

data Fraction = Fraction Int Int
data WavDef = WavDef Int String

data Object = Object {
    bar_ :: Int,
    position :: Fraction,
    channel :: Int,
    index :: String
}

data Eden = Eden {
    bar :: Int,
    bpm :: Double,
    objects :: [Object]
}

data Header = Header {
    title :: String,
    artist :: String
}

data Score = Score {
    header :: Header,
    score :: [Eden],
    wavDef :: [WavDef]
}
