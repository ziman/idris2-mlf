module SortedSet

import Data.Maybe
import Data.SortedMap

export
data SortedSet k = SetWrapper (SortedMap.SortedMap k ())

export
empty : Ord k => SortedSet k
empty = SetWrapper SortedMap.empty

export
insert : k -> SortedSet k -> SortedSet k
insert k (SetWrapper m) = SetWrapper (SortedMap.insert k () m)

export
delete : k -> SortedSet k -> SortedSet k
delete k (SetWrapper m) = SetWrapper (SortedMap.delete k m)

export
contains : k -> SortedSet k -> Bool
contains k (SetWrapper m) = isJust (SortedMap.lookup k m)

export
fromList : Ord k => List k -> SortedSet k
fromList l = SetWrapper (SortedMap.fromList (map (\i => (i, ())) l))

export
toList : SortedSet k -> List k
toList (SetWrapper m) = map (\(i, _) => i) (SortedMap.toList m)

export
Foldable SortedSet where
  foldr f e = foldr f e . SortedSet.toList
  foldl f e = foldl f e . SortedSet.toList
  null (SetWrapper m) = null m

||| Set union. Inserts all elements of x into y
export
union : (x, y : SortedSet k) -> SortedSet k
union x y = foldr insert x y

||| Set difference. Delete all elments in y from x
export
difference : (x, y : SortedSet k) -> SortedSet k
difference x y = foldr delete x y

||| Set symmetric difference. Uses the union of the differences.
export
symDifference : (x, y : SortedSet k) -> SortedSet k
symDifference x y = union (difference x y) (difference y x)

||| Set intersection. Implemented as the difference of the union and the symetric difference.
export
intersection : (x, y : SortedSet k) -> SortedSet k
intersection x y = difference x (difference x y)

export
Ord k => Semigroup (SortedSet k) where
  (<+>) = union

export
Ord k => Monoid (SortedSet k) where
  neutral = empty

export
keySet : SortedMap k v -> SortedSet k
keySet = SetWrapper . map (const ())

export
singleton : Ord k => k -> SortedSet k
singleton k = insert k empty
