module Test.MySolutions where

import Prelude

import ChapterExamples (Amp(..), Volt(..))
import Data.Maybe (Maybe(..))
import Data.Picture (Shape(..), origin, shapeBounds)
import Data.Number (pi)

newtype Watt = Watt Number

factorial :: Int -> Int
factorial 0 = 1
factorial n | n < 0     = 0
            | otherwise = n * factorial (n-1)

-- called as 'binomial n k'
binomial :: Int -> Int -> Int
binomial 0 _ = 0
binomial _ 0 = 1
binomial n k  | n == k    = 1
              | k > n     = 0
              | otherwise = (factorial n) / (factorial k * factorial (n - k))

pascal :: Int -> Int -> Int
pascal 0 _ = 0
pascal _ 0 = 1
pascal n k  | n == k      = 1
            | k > n       = 0
            | otherwise   = pascal (n - 1) k + pascal (n - 1) (k - 1)

-- sameCity :: ∀ (r :: Row Type) (s :: Row Type). { address :: { city :: String | r } | s } -> { address :: { city :: String | r } | s } -> Boolean
sameCity :: ∀ r s. { address :: { city :: String | r } | s } -> { address :: { city :: String | r } | s } -> Boolean
sameCity { address: { city: c1 } } { address: { city: c2 } }  | c1 == c2  = true
                                                              | otherwise = false

fromSingleton :: ∀ a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton y _   = y

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter (Circle _ r) = Circle origin (2.0 * r)
doubleScaleAndCenter (Rectangle _ w h) = Rectangle origin (2.0 * w) (2.0 * h)
doubleScaleAndCenter (Line start end) =
  Line { x: (0.0 - dX), y: (0.0 - dY) } { x: dX, y: dY}
  where
    dX = end.x - start.x
    dY = end.y - start.y
doubleScaleAndCenter (Text _ s) = Text origin s
doubleScaleAndCenter (Clipped pic c w h) =
  Clipped (map doubleScaleAndCenter pic) origin (2.0 * w) (2.0 * h)

shapeText :: Shape -> Maybe String
shapeText (Text _ s) = Just s
shapeText _ = Nothing

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp i) (Volt v) = Watt (i * v)

area :: Shape -> Number
area (Circle _ r) = pi * r * r
area (Rectangle _ w h) = w * h
area _  = 0.0



