module Test.MySolutions where

import Prelude

import Control.Monad.ST (ST, for, run)
import Control.Monad.ST.Internal (modify)
import Control.Monad.ST.Ref (modify, new, read)
import Data.Array (head, tail, foldM, nub, sort)
import Data.Int (toNumber, even)
import Data.List (List(..))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (throwException, error)

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
filterM _ Nil = pure Nil
filterM f (Cons x xs) = do
  tv <- f x
  if tv
    then do
      xs' <- filterM f xs
      pure $ Cons x xs'
    else
      filterM f xs

exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide n d = case d of
  0 -> throwException $ error "div zero"
  _ -> pure $ n / d

estimatePi :: Int -> Number
estimatePi n =
  run do
    ref <- new 1.0
    for 2 (n + 1) \i ->
      let num = toNumber $ if even i then (-1) else 1
          den = toNumber $ 2 * i - 1
      in
        modify (\o -> o + num / den) ref
    final <- read ref
    pure $ 4.0 * final

fibonacci :: Int -> Int
fibonacci n =
  run do
    ref <- new {a: 0, b: 1}
    for 2 (n + 1) \_ ->
      modify (\o ->
          let c = o.a + o.b in
            { a: o.b,
              b: c
            }
        )
        ref
    final <- read ref
    pure final.b


