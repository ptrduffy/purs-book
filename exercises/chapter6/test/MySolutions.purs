module Test.MySolutions where

import Prelude

import Data.Array ((:), nubEq, nub, nubByEq, length)
import Data.Foldable (class Foldable, foldl, foldr, foldMap, maximum)
import Data.Generic.Rep (class Generic)
import Data.Hashable (class Hashable, hashEqual, hash)
import Data.Maybe (fromMaybe)
import Data.Monoid (power)
import Data.Ord (abs)
import Data.Show.Generic (genericShow)

-- Note to reader: Add your solutions to this file

newtype Point
  = Point
  { x :: Number
  , y :: Number
  }

instance Show Point where
  show (Point {x, y}) = "(" <> show x <> ", " <> show y <> ")"

newtype Complex
  = Complex
  { real :: Number
  , imaginary :: Number
  }

instance Show Complex where
  show (Complex {real, imaginary}) = show real <> sign <> show (abs imaginary) <> "i"
    where sign = if imaginary >= 0.0 then "+" else "-"

derive instance eqComplex :: Eq Complex

{-
instance Semiring Complex where
  zero = Complex {real: 0.0, imaginary: 0.0}
  one = Complex {real: 1.0, imaginary: 0.0}
  add (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2})
    = Complex {real: r1 + r2, imaginary: i1 + i2}
  mul (Complex {real: r1, imaginary: i1}) (Complex {real: r2, imaginary: i2})
    = Complex {real: r1 * r2 + (-1.0) * i1 * i2, imaginary: i1 * r2 + i2 * r1}
-}

instance Semiring Complex where
  zero = Complex {real: 0.0, imaginary: 0.0}
  one = Complex {real: 1.0, imaginary: 0.0}
  add (Complex c1) (Complex c2) = Complex {real: c1.real + c2.real, imaginary: c1.imaginary + c2.imaginary}
  mul (Complex c1) (Complex c2) = Complex{real: c1.real * c2.real + (-1.0) * c1.imaginary * c2.imaginary,
                                          imaginary: c1.imaginary * c2.real + c2.imaginary * c1.real}

derive newtype instance ringComplex :: Ring Complex

data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
  show = genericShow

data NonEmpty a = NonEmpty a (Array a)

derive instance eqNonEmpty :: Eq a => Eq (NonEmpty a)

derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance Show a => Show (NonEmpty a) where
  show = genericShow

nel :: NonEmpty Int
nel = NonEmpty 1 [2, 3, 4]

instance Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> y:ys)

-- Unlike Semigroup above, Functor doesn't need or want the (NonEmpty a) syntax,
-- just 'NonEmpty' - I think this is because Functor is defined as taking a
-- (Type -> Type) kind as its first parameter (which is what NonEmpty is - whereas
-- (NonEmpty a) is a concrete Type)
instance Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

data Extended a = Infinite | Finite a

derive instance Eq a => Eq (Extended a)

-- Need the Eq instance above for Ord to work
instance Ord a => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite (Finite _) = GT
  compare (Finite _) Infinite = LT
  compare (Finite x) (Finite y) = compare x y

-- 'NonEmpty' vs (NonEmpty a) for same reasons as Functor above
instance Foldable NonEmpty where
  foldr :: ∀ a b. (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f acc (NonEmpty x xs) = foldr f acc (x:xs)
  foldl :: ∀ a b. (b -> a -> b) -> b -> NonEmpty a -> b
  foldl f acc (NonEmpty x xs) = foldl f acc (x:xs)
  foldMap :: ∀ a m. Monoid m => (a -> m) -> NonEmpty a -> m
  foldMap ff (NonEmpty x xs) = foldMap ff (x:xs)

data OneMore f a = OneMore a (f a)

-- (OneMore f) vs (OneMore f a) for same reasons as Functor
-- i.e. (OneMore f) has Kind (Type -> Type)
instance Foldable f => Foldable (OneMore f) where
  -- foldr - first run foldr over existing Foldable fx, getting total in acc
  -- then apply supplied fctn f to extra x and acc
  -- foldr :: ∀ a b. (a -> b -> b) -> b -> f a -> b
  foldr f acc (OneMore x fx) = f x (foldr f acc fx)
  -- foldl - first apply supplied fctn f to x to get starting acc
  -- then run foldl with this acc over existing Foldable
  -- foldl :: ∀ a b. (b -> a -> b) -> b -> f a -> b
  foldl f acc (OneMore x fx) = foldl f (f acc x) fx
  -- foldMap - Monoid contraint implies Semigroup constraint, so append is available
  -- just need to apply supplied fctn f to x and append to foldMap on orig Foldable
  -- foldMap :: ∀ a m. Monoid m => (a -> m) -> f a -> m
  foldMap ff (OneMore x fx) =  ff x <> foldMap ff fx

-- for nubEq in dedupShapes
derive newtype instance Eq Point
derive instance Eq Shape

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

-- Pretty wild that Ord can be derived?!
-- and this is all that's needed for nub support
derive newtype instance Ord Point
derive instance Ord Shape

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum xs = fromMaybe 0 $ maximum xs

class Monoid m <= Action m a where
  act :: m -> a -> a

newtype Multiply = Multiply Int

derive newtype instance Show Multiply
derive newtype instance Eq Multiply

instance Semigroup Multiply where
  append (Multiply n) (Multiply m) = Multiply (n * m)

instance Monoid Multiply where
  mempty = Multiply 1

instance Action Multiply Int where
  act (Multiply n) i = n * i

instance Action Multiply String where
  act (Multiply n) s = power s n

instance Action m a => Action m (Array a) where
  act m xs = map (act m) xs

newtype Self m = Self m

derive newtype instance Show m => Show (Self m)
derive newtype instance Eq m => Eq (Self m)

instance Monoid m => Action m (Self m) where
  act m1 (Self m2) = Self (m1 <> m2)

arrayHasDuplicates :: ∀ a. Hashable a => Array a -> Boolean
arrayHasDuplicates xs = if length xs' /= (length xs) then true else false
  where
    xs' = nubByEq hashAndValEqual xs
      where
        hashAndValEqual x y = (hashEqual x y) && (x == y)

newtype Hour = Hour Int

instance Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12

fromHour :: Hour -> Int
fromHour (Hour h) = h `mod` 12

instance Hashable Hour where
  hash = hash <<< fromHour

