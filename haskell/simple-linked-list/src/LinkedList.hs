module LinkedList
    ( LinkedList
    , datum
    , fromList
    , isNil
    , new
    , next
    , nil
    , reverseLinkedList
    , toList
    ) where

import Data.List
import Data.Maybe

data LinkedList a = LinkedList { current :: Maybe a, maybeNext :: Maybe (LinkedList a) } deriving (Eq, Show)

datum :: LinkedList a -> a
datum = fromJust . current

next :: LinkedList a -> LinkedList a
next = fromJust . maybeNext

fromList :: [a] -> LinkedList a
fromList = foldl (flip new) nil . reverse

isNil :: LinkedList a -> Bool
isNil LinkedList { current = Nothing } = True
isNil _                                = False

new :: a -> LinkedList a -> LinkedList a
new x linkedList = LinkedList { current = Just x, maybeNext = Just linkedList }

nil :: LinkedList a
nil = LinkedList { current = Nothing, maybeNext = Nothing }

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = fromList . reverse . toList

toList :: LinkedList a -> [a]
toList LinkedList { current = (Just x), maybeNext = (Just n) } = x : toList n
toList LinkedList { current = Nothing }                        = []
