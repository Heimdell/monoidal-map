
module Data.Map.Monoidal where

import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Coerce
import Data.Maybe

newtype Map k v = MkMap
  { impl :: Map.Map k v
  }
  deriving newtype (Eq, Ord, Show, Functor, Foldable)

instance (Ord k, Semigroup v) => Semigroup (Map k v) where
  l <> r =
    coerce @(Map.Map _ v) (Map.unionWith (<>) (coerce l) (coerce r))

instance (Ord k, Semigroup v) => Monoid (Map k v) where
  mempty = coerce @(Map.Map _ v) Map.empty

insert :: (Ord k, Semigroup v) => k -> v -> Map k v -> Map k v
insert k v = coerce . Map.insertWith (<>) k v . coerce

singleton :: (Ord k) => k -> v -> Map k v
singleton = (coerce .) . Map.singleton

(!) :: (Ord k, Monoid v) => Map k v -> k -> v
(!) = (fromMaybe mempty .) . flip Map.lookup . coerce

map :: (v -> w) -> Map k v -> Map k w
map = (coerce .) . (. coerce) . Map.map

filter :: (Ord k) => (v -> Bool) -> Map k v -> Map k v
filter = (coerce .) . (. coerce) . Map.filter

toList :: Map k v -> [(k, v)]
toList = Map.toList . coerce

from, fromList :: (Ord k, Semigroup v) => [(k, v)] -> Map k v
from = foldr (uncurry insert) mempty
fromList = from

keys :: forall k v. Map k v -> Set.Set k
keys = Map.keysSet . coerce @_ @(Map.Map _ v)

member :: forall k v. (Ord k) => k -> Map k v -> Bool
member = (. coerce @_ @(Map.Map _ v)) . Map.member

memoise
  :: ( Foldable t
     , Ord k
     , Semigroup v
     )
  => t k
  -> (k -> v)
  -> Map k v
memoise keys f = foldr (\k -> insert k (f k)) mempty keys