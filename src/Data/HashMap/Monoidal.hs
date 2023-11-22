
module Data.HashMap.Monoidal where

import Data.HashMap.Strict qualified as Map
import Data.HashSet qualified as Set
import Data.Coerce
import Data.Maybe
import Data.Hashable

newtype Map k v = MkMap
  { impl :: Map.HashMap k v
  }
  deriving newtype (Eq, Ord, Show, Functor, Foldable)

instance (Hashable k, Semigroup v) => Semigroup (Map k v) where
  l <> r =
    coerce @(Map.HashMap _ v) (Map.unionWith (<>) (coerce l) (coerce r))

instance (Hashable k, Semigroup v) => Monoid (Map k v) where
  mempty = coerce @(Map.HashMap _ v) Map.empty

insert :: (Hashable k, Semigroup v) => k -> v -> Map k v -> Map k v
insert k v = coerce . Map.insertWith (<>) k v . coerce

singleton :: (Hashable k) => k -> v -> Map k v
singleton = (coerce .) . Map.singleton

(!) :: (Hashable k, Monoid v) => Map k v -> k -> v
(!) = (fromMaybe mempty .) . flip Map.lookup . coerce

map :: (v -> w) -> Map k v -> Map k w
map = (coerce .) . (. coerce) . Map.map

filter :: (v -> Bool) -> Map k v -> Map k v
filter = (coerce .) . (. coerce) . Map.filter

toList :: Map k v -> [(k, v)]
toList = Map.toList . coerce

from, fromList :: (Hashable k, Semigroup v) => [(k, v)] -> Map k v
from = foldr (uncurry insert) mempty
fromList = from

keys :: forall k v. Map k v -> Set.HashSet k
keys = Map.keysSet . coerce @_ @(Map.HashMap _ v)

member :: forall k v. (Hashable k) => k -> Map k v -> Bool
member = (. coerce @_ @(Map.HashMap _ v)) . Map.member

memoise
  :: ( Foldable t
     , Hashable k
     , Semigroup v
     )
  => t k
  -> (k -> v)
  -> Map k v
memoise keys f = foldr (\k -> insert k (f k)) mempty keys