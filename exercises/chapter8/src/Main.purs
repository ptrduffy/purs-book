module Main where

import Prelude

import Data.AddressBook (PhoneNumber, PhoneType, examplePerson, phoneNumber)
import Data.AddressBook.Validation (Errors, Field(..), ValidationError(..), validatePerson')
import Data.Array (filter, head, mapMaybe, mapWithIndex, updateAt, (..))
import Data.Bounded.Generic (class GenericBottom, class GenericTop, genericBottom, genericTop)
import Data.Either (Either(..))
import Data.Enum.Generic (class GenericBoundedEnum, genericFromEnum, genericToEnum)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import React.Basic.DOM as D
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, element, reactComponent, useState)
import React.Basic.Hooks as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)


-- Note that there's a Purty formatting bug that
-- adds an unwanted blank line
-- https://gitlab.com/joneshf/purty/issues/77
renderValidationErrors :: Errors -> Array R.JSX
renderValidationErrors [] = []
renderValidationErrors xs =
  let
    renderError :: ValidationError -> R.JSX
    -- renderError err = D.div_ [ D.text err ]
    -- change for Ex 2
    renderError err = D.div
                        { className: "alert alert-danger row"
                        , children: [ D.text $ show err ]
                        }
  in
    {-
    [ D.div
        { className: "alert alert-danger row"
        , children: [ D.ul_ (map renderError xs) ]
        }
    ]
    -}
    -- change for Ex 2
    map renderError xs

-- Helper function to render a single form field with an
-- event handler to update
formField :: String -> String -> String -> String -> (String -> Effect Unit) -> R.JSX
formField name placeholder value error setValue =
  D.div
    { className: "form-group row"
    , children:
        [ D.label
            { className: "col-sm col-form-label"
            , htmlFor: name
            , children: [ D.text name ]
            }
        , D.div
            { className: "col-sm"
            , children:
                [ D.input
                    { className: "form-control"
                    , id: name
                    , placeholder
                    , value
                    , onChange:
                        let
                          handleValue :: Maybe String -> Effect Unit
                          handleValue (Just v) = setValue v
                          handleValue Nothing  = pure unit
                        in
                          handler targetValue handleValue
                    }
                ]
            }
        , D.text error
        ]
    }

-- Helper function to enumerate all elements of the PhoneNumber type
-- from https://stackoverflow.com/questions/68246044/get-array-containing-all-data-type-posible-values
allElements ::
  ∀ a rep.
  Generic a rep =>
  GenericBoundedEnum rep =>
  GenericTop rep =>
  GenericBottom rep =>
  Array a
allElements = mapMaybe genericToEnum (idxFrom..idxTo)
  where
    idxFrom = genericFromEnum (genericBottom :: a)
    idxTo = genericFromEnum (genericTop :: a)

-- Note usage below - don't need to pass PhoneType, compiler handles it
allPhoneTypes :: Array PhoneType
allPhoneTypes = allElements

getPhoneNumber :: Array PhoneNumber -> PhoneType -> Maybe PhoneNumber
getPhoneNumber pns pt = head $ filter (\pn -> pn."type" == pt) pns

getPNWithDefault :: String -> Array PhoneNumber -> PhoneType -> PhoneNumber
getPNWithDefault def pns pt = fromMaybe (phoneNumber pt def) (getPhoneNumber pns pt)

errorForField :: Field -> Array ValidationError -> String
errorForField f errs =
  fromMaybe "" $
  head $
  map (\(ValidationError str _) -> str) $
  filter ( \(ValidationError _ fld) -> f == fld ) errs 


mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  -- incoming \props are unused
  reactComponent "AddressBookApp" \_ -> R.do
    -- `useState` takes a default initial value and returns the
    -- current value and a way to update the value.
    -- Consult react-hooks docs for a more detailed explanation of `useState`.
    Tuple person setPerson <- useState examplePerson
    let
      errors = case validatePerson' person of
        Left  e -> e
        Right _ -> []

      -- helper-function to return array unchanged instead of Nothing if index is out of bounds
      updateAt' :: forall a. Int -> a -> Array a -> Array a
      updateAt' i x xs = fromMaybe xs (updateAt i x xs)

      -- helper-function to render a single phone number at a given index
      renderPhoneNumber :: Int -> PhoneNumber -> R.JSX
      renderPhoneNumber index phone =
        formField
          (show phone."type")
          "XXX-XXX-XXXX"
          phone.number
          (errorForField (PhoneField phone."type") errors)
          (\s -> setPerson _ { phones = updateAt' index phone { number = s } person.phones })
        
      -- helper-function to render all phone numbers
      -- inner 'map ...' used to shaw all phone number regardless of what Person contains
      renderPhoneNumbers :: Array R.JSX
      renderPhoneNumbers = mapWithIndex renderPhoneNumber (map (getPNWithDefault "XXX-XXX-XXXX" person.phones) allPhoneTypes)
    pure
      $ D.div
          { className: "container"
          , children:
              -- renderValidationErrors errors
              --  <> 
                [ D.div
                      { className: "row"
                      , children:
                          [ D.form_
                              $ [ D.h3_ [ D.text "Basic Information" ]
                                , formField "First Name" "First Name" person.firstName (errorForField FirstNameField errors) \s ->
                                    setPerson _ { firstName = s }
                                , formField "Last Name" "Last Name" person.lastName (errorForField LastNameField errors) \s ->
                                    setPerson _ { lastName = s }
                                , D.h3_ [ D.text "Address" ]
                                , formField "Street" "Street" person.homeAddress.street (errorForField StreetField errors) \s ->
                                    setPerson _ { homeAddress { street = s } }
                                , formField "City" "City" person.homeAddress.city (errorForField CityField errors) \s ->
                                    setPerson _ { homeAddress { city = s } }
                                , formField "State" "State" person.homeAddress.state (errorForField StateField errors) \s ->
                                    setPerson _ { homeAddress { state = s } }
                                , D.h3_ [ D.text "Contact Information" ]
                                ]
                              <> renderPhoneNumbers
                              -- Ex 1 - show work phone number
                              -- <> [formField "Work" "XXX-XXX-XXXX" "XXX-XXX-XXXX"
                              --    (\s -> log $ "Setting work phone to " <> s)]
                          ]
                      , key: "person-form"
                      }
                  ]
          }


{-
mkAddressBookApp :: Effect (ReactComponent {})
mkAddressBookApp =
  reactComponent
    "AddressBookApp"
    (\_ -> pure $ D.text "Hi! I'm an address book")
-}

main :: Effect Unit
main = do
  log "Rendering address book component"
  -- Get window object
  w <- window
  -- Get window's HTML document
  doc <- document w
  -- Get "container" element in HTML
  ctr <- getElementById "container" $ toNonElementParentNode doc
  case ctr of
    Nothing -> throw "Container element not found."
    Just c -> do
      -- Create AddressBook react component
      addressBookApp <- mkAddressBookApp
      let
        -- Create JSX node from react component. Pass-in empty props
        app = element addressBookApp {}
      -- Render AddressBook JSX node in DOM "container" element
      D.render app c
