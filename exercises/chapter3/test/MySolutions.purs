module Test.MySolutions where

import Prelude

import Data.AddressBook (Address, Entry, AddressBook)
import Data.List (filter, head, nubByEq, (:))
import Data.Maybe (Maybe)

-- Note to reader: Add your solutions to this file
findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet streetName = head <<< filter (eq streetName <<< _.address.street)

isInBook :: String -> String -> AddressBook -> Boolean
isInBook fn ln (e:es) = if e.firstName == fn && e.lastName == ln
                      then true
                      else isInBook fn ln es
isInBook _ _ _   = false

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubByEq filterEntry
  where
  filterEntry :: Entry -> Entry -> Boolean
  filterEntry e1 e2 = if e1.firstName == e2.firstName && e1.lastName == e2.lastName
                      then true
                      else false
                      

