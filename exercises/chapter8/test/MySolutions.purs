module Test.MySolutions where

import Prelude

import Data.Array (head, tail, foldM, nub, sort)
import Data.Maybe (Maybe)


-- Note to reader: Add your solutions to this file

third :: ∀ a. Array a -> Maybe a
third xs = do
  ys <- tail xs
  zs <- tail ys
  head zs

-- Originally had (\x y -> [x, y, x + y]), but don't need the 'y'
-- as it will be included in the next iteration
possibleSums :: Array Int -> Array Int
possibleSums [] = [0]
possibleSums [x] = [x]
possibleSums xs = (nub <<< sort) $ foldM (\x y -> [x, x + y]) 0 xs

filterM :: forall m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM f xs = 

  