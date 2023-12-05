module Test.MySolutions where

import Prelude

import Data.Number (sqrt, pi)
import Data.Int (rem)

diagonal :: Number → Number → Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

leftoverCents :: Int -> Int
leftoverCents c = rem c 100
