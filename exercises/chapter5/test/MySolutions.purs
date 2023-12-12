module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (catMaybes, cons, filter, head, length, partition, tail, (..), (:), sortBy, last)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path(..), filename, isDirectory, ls)
import Test.Examples (factors, allFiles)

-- import Data.Number ((%))

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
isEven x = case x `mod` 2 of
            0 -> true
            _ -> false

countEven :: Array Int -> Int
countEven [] = 0
countEven xs =
  case isEven $ fromMaybe 0 $ head xs of
  true -> 1 + (countEven $ fromMaybe [] $ tail xs)
  false -> countEven $ fromMaybe [] $ tail xs

squared :: Array Number -> Array Number
squared ns = map (\n -> n * n) ns

keepNonNegative :: Array Number -> Array Number
-- can use the "_" notation below as we don't need to use 'n' on the RHS
keepNonNegative ns = filter ( _ >= 0.0 ) ns

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime n = (length $ factors n) == 1

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct [] _ = []
cartesianProduct _ [] = []
cartesianProduct a1 a2 = do
  i <- a1
  j <- a2
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

primeFactors :: Int -> Array Int
primeFactors n = factorise n 2
  where
    factorise :: Int -> Int -> Array Int
    factorise 1 _ = []
    factorise m d = 
      if m `mod` d == 0 then
        d : factorise (m/d) d
      else
        factorise m (d + 1)

allTrue :: Array Boolean -> Boolean
allTrue arr = foldl (&&) true arr

fibTailRec :: Int -> Int
fibTailRec 0 = 0
fibTailRec 1 = 1
fibTailRec n = fibTR n 2 1 0
  where
    fibTR :: Int -> Int -> Int -> Int -> Int
    fibTR target pos prev pprev
      | target == pos = prev + pprev
      | otherwise     = fibTR target (pos + 1) (prev + pprev) prev

reverse :: ∀ a. Array a -> Array a
reverse = foldl (flip cons) []

onlyFiles :: Path -> Array Path
onlyFiles path  | not isDirectory path  = []
                | otherwise             = do
                    let {no: fs, yes: ds} = partition isDirectory (ls path)
                    fs <> do
                      d <- ds
                      onlyFiles d

{-
This approach was ultimately doomed because "whereIs" returns something in a
"Maybe" context, whereas the "do" block is trying to work with items in an
"Array" context

whereIs :: Path -> String -> Maybe Path
whereIs (File _ _) _ = Nothing
whereIs dir@(Directory dname conts) fname =
  let {no: fs, yes: ds} = partition isDirectory conts in
    if fname `elem` (map filename fs)
    then Just dir
    else do
      d <- ds                   -- Array
      pure $ whereIs d fname    -- Maybe
-}

whereIs :: Path -> String -> Maybe Path
whereIs (File _ _) _ = Nothing
whereIs dir fname = head $ do
  p <- allFiles dir
  fn <- map filename (ls p)    -- returns [] when called on a file
  -- let fnames = map filename fs -- doesn't work for some reason?  Limitations of 'let' in a 'do' block?
  guard $ filename p <> fname == fn
  pure p

cmpFSize :: Path -> Path -> Ordering
cmpFSize (File _ s1) (File _ s2) = compare s1 s2
cmpFSize _ _ = EQ

largestSmallest :: Path -> Array Path
largestSmallest path = 
  let fs = sortBy cmpFSize $ onlyFiles path in
    case length fs of
      0 -> []
      1 -> fs
      _ -> catMaybes [head fs, last fs]




