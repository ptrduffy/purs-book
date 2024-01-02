module Data.AddressBook.Validation where

import Prelude

import Data.AddressBook (Address, Person, PhoneNumber, PhoneType, address, person, phoneNumber)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (length)
import Data.String.Regex (Regex, test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (traverse)
import Data.Validation.Semigroup (V, invalid, toEither)

{-
type Errors
  = Array String
-}

-- below added for Ex 3

data Field = FirstNameField
           | LastNameField
           | StreetField
           | CityField
           | StateField
           | PhoneNumsField
           | PhoneField PhoneType

derive instance Eq Field

derive instance Generic Field _

instance Show Field where
  show = genericShow

data ValidationError = ValidationError String Field

derive instance Generic ValidationError _

instance Show ValidationError where
  show = genericShow

type Errors = Array ValidationError

nonEmpty :: Field -> String -> V Errors String
nonEmpty field ""     = invalid [ ValidationError ("Field '" <> show field <> "' cannot be empty") field]
nonEmpty _     value  = pure value

validatePhoneNumbers :: Field -> Array PhoneNumber -> V Errors (Array PhoneNumber)
validatePhoneNumbers field []      =
  invalid [ ValidationError ("Field '" <> show field <> "' must contain at least one value") field ]
validatePhoneNumbers _     phones  =
  traverse validatePhoneNumber phones

lengthIs :: Field -> Int -> String -> V Errors String
lengthIs field len value | length value /= len =
  invalid [ ValidationError ("Field '" <> show field <> "' must have length " <> show len) field ]
lengthIs _     _   value = pure value

phoneNumberRegex :: Regex
phoneNumberRegex = unsafeRegex "^\\d{3}-\\d{3}-\\d{4}$" noFlags

matches :: Field -> Regex -> String -> V Errors String
matches _     regex value | test regex value 
                          = pure value
matches field _     _     = invalid [ ValidationError ("Field '" <> show field <> "' did not match the required format") field ]

validateAddress :: Address -> V Errors Address
validateAddress a =
  address <$> nonEmpty StreetField  a.street
          <*> nonEmpty CityField    a.city
          <*> lengthIs StateField 2 a.state

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
  phoneNumber <$> pure pn."type"
              <*> matches (PhoneField pn."type") phoneNumberRegex pn.number

validatePerson :: Person -> V Errors Person
validatePerson p =
  person <$> nonEmpty FirstNameField p.firstName
         <*> nonEmpty LastNameField p.lastName
         <*> validateAddress p.homeAddress
         <*> validatePhoneNumbers PhoneNumsField p.phones

validatePerson' :: Person -> Either Errors Person
validatePerson' p = toEither $ validatePerson p
