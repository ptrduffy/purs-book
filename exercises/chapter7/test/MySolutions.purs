module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Person, Address, PhoneNumber, person, address)
import Data.AddressBook.Validation (Errors, matches, nonEmpty, validatePhoneNumbers, validateAddress)
import Data.Generic.Rep (class Generic)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Data.Validation.Semigroup (V)

addApply :: ∀ a f. Semiring a => Apply f => f a -> f a -> f a
addApply = lift2 add

addMaybe :: ∀ a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = addApply

mulApply :: ∀ a f. Semiring a => Apply f => f a -> f a -> f a
mulApply = lift2 mul

mulMaybe :: ∀ a. Semiring a => Maybe a -> Maybe a -> Maybe a
mulMaybe = mulApply

subApply :: ∀ a f. Ring a => Apply f => f a -> f a -> f a
subApply = lift2 sub

subMaybe :: ∀ a. Ring a => Maybe a -> Maybe a -> Maybe a
subMaybe = subApply

divApply :: ∀ a f. EuclideanRing a => Apply f => f a -> f a -> f a
divApply = lift2 div

divMaybe ∷ ∀ a. EuclideanRing a => Maybe a -> Maybe a -> Maybe a
divMaybe = divApply

combineMaybe :: ∀ a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just x) = map Just x
combineMaybe Nothing = pure Nothing

{-
The following are concrete implementations that work for
Maybe (List) and Maybe (Array) - used these to try and
figure out the more general Applicative above

combineMaybe :: ∀ a. Maybe (List a) -> List (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just ls) = map Just ls

combineMaybe :: ∀ a. Maybe (Array a) -> Array (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just xs) = map Just xs
-}


stateRegex :: Regex
stateRegex = unsafeRegex "^[a-zA-Z]{2}$" noFlags

nonEmptyRegex :: Regex
nonEmptyRegex = unsafeRegex "\\S" noFlags

validatePersonImproved :: Person -> V Errors Person
validatePersonImproved p =
  person <$> nonEmpty "First Name" p.firstName
         <*> nonEmpty "Last Name" p.lastName
         <*> validateAddressImproved p.homeAddress
         <*> validatePhoneNumbers "Phone Numbers" p.phones

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a =
  address <$> matches "Street" nonEmptyRegex a.street
          <*> matches "City" nonEmptyRegex a.city
          <*> matches "State" stateRegex a.state

data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance Eq a => Eq (Tree a)
derive instance Generic (Tree a) _

-- the eta-expanded version (show t instead of just show, same on RHS)
-- is needed to prevent stack overflows with recursive ADTs
-- https://github.com/purescript/documentation/blob/master/guides/Type-Class-Deriving.md
instance Show a => Show (Tree a) where
  show t = genericShow t

instance Functor Tree where
  map _ Leaf = Leaf
  map f (Branch lt n rt) = Branch (map f lt) (f n) (map f rt)
 
instance Foldable Tree where
  foldl :: forall a b. (b -> a -> b) -> b -> Tree a -> b
  foldl _ acc Leaf = acc
  foldl f acc (Branch lt n rt) = foldl f (f (foldl f acc lt) n) rt
  foldr :: forall a b. (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr f acc (Branch lt n rt) = foldr f (f n (foldr f acc rt)) lt
  foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch lt n rt) = foldMap f lt <> f n <> foldMap f rt

-- Took ages to figure out the below (and I had to peek at the answer)
-- Key learnings:
--
-- 1. In thinking about how to go from 'Tree (m a)' to 'm (Tree a)' I was
-- trying to think of how to get the 'a' out of the (m a), which can't be
-- done in a generic fashion (if we know it was, e.g., (Maybe a) then we
-- could match on 'Just x' and 'Nothing', but can't do that for generic m).
-- instead the solution is to lift the 'Tree' into the m with map (<$>).
-- This emphasises that Type Constructors are just functions and so can be
-- used wherever functions could
--
-- 2. In the various validate* examples above, the <$>...<*> pattern is used
-- where <$> gets us a partially applied f inside the m, and <*> is subsequently
-- used to apply to successive arguments.  Clearly something similar is
-- happening here, but still not sure I quite understand how.
--
-- 
--
instance Traversable Tree where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch lt n rt) = Branch <$> (traverse f lt) <*> f n <*> (traverse f rt)

  sequence :: ∀ a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch lt n rt) = Branch <$> sequence lt <*> n <*> (sequence rt)

-- used the below as an example of a simpler traverse to
-- help in working out the above
traverseMaybe :: ∀ a b. (a -> Maybe b) -> List a -> Maybe (List b)
traverseMaybe _ Nil = pure Nil
traverseMaybe f (Cons x xs) = Cons <$> f x <*> (traverseMaybe f xs)

-- Note that the wrapped Tree has the same structure as above, the only
-- difference is the order in which the 'effects' are executed
traversePreOrder :: ∀ a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePreOrder _ Leaf = pure Leaf
traversePreOrder f (Branch lt n rt) = ado
  root <- f n
  left <- traversePreOrder f lt
  right <- traversePreOrder f rt
  in Branch left root right

traversePostOrder :: ∀ a m b. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
traversePostOrder _ Leaf = pure Leaf
traversePostOrder f (Branch lt n rt) = ado
  left <- traversePostOrder f lt
  right <- traversePostOrder f rt
  root <- f n
  in Branch left root right

-- I initially thought the following was a more sensible Tree implementation
-- but then it makes it a bit weird as the root can't have a value
-- and actually it doesn't capture Tree properly as you can't
-- navigate through a path of values cos every value ends the Tree
--
-- The above implementation is better, although it might be clearer if
-- 'Leaf' was renamed 'Empty' or 'DeadEnd' or something
data BTree a = Value a | Node (Tree a) (Tree a)

derive instance Eq a => Eq (BTree a)
derive instance Generic (BTree a) _

instance Show a => Show (BTree a) where
  show t = genericShow t

instance Functor BTree where
  map f (Value a) = Value (f a)
  map f (Node lt rt) = Node (map f lt) (map f rt)

instance Foldable BTree where
  foldl f acc (Value v) = f acc v
  foldl f acc (Node lt rt) = foldl f (foldl f acc lt) rt
  foldr f acc (Value v) = f v acc
  foldr f acc (Node lt rt) = foldr f (foldr f acc rt) lt
  foldMap f (Value v) = f v
  foldMap f (Node lt rt) = foldMap f lt <> foldMap f rt

type PersonOptionalAddress
  = { firstName :: String
    , lastName :: String
    , homeAddress :: Maybe Address
    , phones :: Array PhoneNumber
    }

personOptionalAddress :: String -> String -> Maybe Address -> Array PhoneNumber -> PersonOptionalAddress
personOptionalAddress firstName lastName homeAddress phones = { firstName, lastName, homeAddress, phones }

validatePersonOptionalAddress :: PersonOptionalAddress -> V Errors PersonOptionalAddress
validatePersonOptionalAddress p =
  personOptionalAddress
    <$> nonEmpty "First Name" p.firstName
    <*> nonEmpty "Last Name" p.lastName
    <*> traverse validateAddress p.homeAddress
    <*> validatePhoneNumbers "Phone Numbers" p.phones

sequenceUsingTraverse :: ∀ a m t. Traversable t => Applicative m => t (m a) -> m (t a)
sequenceUsingTraverse t = traverse identity t

traverseUsingSequence :: ∀ a b m t. Traversable t => Applicative m => (a -> m b) -> t a -> m (t b)
traverseUsingSequence f t = sequence $ map f t

