{-# LANGUAGE DeriveDataTypeable #-}
module Covid where

import Text.JSON.Generic

data Datapoint = Datapoint {
	country :: String,
	active :: Int,
	city :: String,
	citycode :: String,
  confirmed :: Int,
  date :: String,
  deaths :: Int,
  lat :: String,
  lon :: String,
  province :: String,
  recovered :: Int
} deriving (Typeable, Data, Show, Eq)
