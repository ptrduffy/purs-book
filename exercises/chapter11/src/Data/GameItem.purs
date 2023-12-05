module Data.GameItem where

import Prelude

import Data.Maybe (Maybe(..))

data GameItem = Candle | Matches

instance Show GameItem where
  show Candle         = "Candle"
  show Matches        = "Matches"

derive instance Eq GameItem
derive instance Ord GameItem

readItem :: String -> Maybe GameItem
readItem "Candle" = Just Candle
readItem "Matches" = Just Matches
readItem _ = Nothing
