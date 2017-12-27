module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy, null)
import Data.Maybe (Maybe)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


-- 1. (Easy) Test your understanding of the findEntry function by writing down
-- the types of each of its major subexpressions. For example, the type of the
-- head function as used is specialized to AddressBook -> Maybe Entry.

-- filterEntry :: Entry -> Boolean
-- filter :: forall a. (a -> Boolean) - List a -> List a
-- filter filterEntry :: List Entry -> List Entry
-- head :: forall a. List a -> List a
-- (<<<) :: forall b c d a. Semigroupoid a => a c d -> a b c -> a b d
-- head <<< filter filterEntry :: Entry


-- 2. (Medium) Write a function which looks up an Entry given a street address,
-- by reusing the existing code in findEntry. Test your function in PSCi.

findByAddr :: String -> AddressBook -> Maybe Entry
findByAddr addr = head <<< filter filterEntry
    where
    filterEntry :: Entry -> Boolean
    filterEntry entry = entry.address.street == addr


-- 3. (Medium) Write a function which tests whether a name appears in a
-- AddressBook, returning a Boolean value. Hint: Use PSCi to find the type of
-- the Data.List.null function, which test whether a list is empty or not.

nameInAddressBook :: String -> String -> AddressBook -> Boolean
nameInAddressBook firstName lastName = not <<< null <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName


-- 4. (Difficult) Write a function removeDuplicates which removes duplicate
-- address book entries with the same first and last names. Hint: Use PSCi to
-- find the type of the Data.List.nubBy function, which removes duplicate
-- elements from a list based on an equality predicate.

removeDupeEntries :: AddressBook -> AddressBook
removeDupeEntries = nubBy isDupe
    where
    isDupe :: Entry -> Entry -> Boolean
    isDupe a b = a.firstName == b.firstName && a.lastName == b.lastName
