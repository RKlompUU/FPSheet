{-# LANGUAGE DeriveDataTypeable #-}
module Covid where

import Text.JSON.Generic

data Datapoint = Datapoint {
  country :: String,
  active :: Double,
  city :: String,
  citycode :: String,
  confirmed :: Double,
  date :: String,
  deaths :: Double,
  lat :: String,
  lon :: String,
  province :: String,
  recovered :: Double
} deriving (Typeable, Data, Show, Eq)

decode :: String -> [Datapoint]
decode = decodeJSON
