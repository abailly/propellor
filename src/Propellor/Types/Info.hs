{-# LANGUAGE GADTs, DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Propellor.Types.Info (
	Info,
	IsInfo(..),
	addInfo,
	toInfo,
	fromInfo,
	mapInfo,
	propagatableInfo,
	InfoVal(..),
	fromInfoVal,
	Typeable,
) where

import Data.Dynamic
import Data.Maybe
import Data.Monoid
import Prelude

-- | Information about a Host, which can be provided by its properties.
--
-- Many different types of data can be contained in the same Info value
-- at the same time. See `toInfo` and `fromInfo`.
newtype Info = Info [InfoEntry]
	deriving (Monoid, Show)

data InfoEntry where
	InfoEntry :: (IsInfo v, Typeable v) => v -> InfoEntry

instance Show InfoEntry where
	show (InfoEntry v) = show v

-- Extracts the value from an InfoEntry but only when
-- it's of the requested type.
extractInfoEntry :: Typeable v => InfoEntry -> Maybe v
extractInfoEntry (InfoEntry v) = cast v

-- | Values stored in Info must be members of this class.
--
-- This is used to avoid accidentially using other data types
-- as info, especially type aliases which coud easily lead to bugs.
-- We want a little bit of dynamic types here, but not too far..
class (Typeable v, Monoid v, Show v) => IsInfo v where
	-- | Should info of this type be propagated out of a
	-- container to its Host?
	propagateInfo :: v -> Bool

-- | Any value in the `IsInfo` type class can be added to an Info.
addInfo :: IsInfo v => Info -> v -> Info
addInfo (Info l) v = Info (InfoEntry v:l)

-- | Converts any value in the `IsInfo` type class into an Info,
-- which is otherwise empty.
toInfo :: IsInfo v => v -> Info
toInfo = addInfo mempty

-- The list is reversed here because addInfo builds it up in reverse order.
fromInfo :: IsInfo v => Info -> v
fromInfo (Info l) = mconcat (mapMaybe extractInfoEntry (reverse l))

-- | Maps a function over all values stored in the Info that are of the
-- appropriate type.
mapInfo :: IsInfo v => (v -> v) -> Info -> Info
mapInfo f (Info l) = Info (map go l)
  where
	go i = case extractInfoEntry i of
		Nothing -> i
		Just v -> InfoEntry (f v)

-- | Filters out parts of the Info that should not propagate out of a
-- container.
propagatableInfo :: Info -> Info
propagatableInfo (Info l) = Info (filter (\(InfoEntry a) -> propagateInfo a) l)

-- | Use this to put a value in Info that is not a monoid.
-- The last value set will be used. This info does not propagate
-- out of a container.
data InfoVal v = NoInfoVal | InfoVal v
	deriving (Typeable, Show)

instance Monoid (InfoVal v) where
	mempty = NoInfoVal
	mappend _ v@(InfoVal _) = v
	mappend v NoInfoVal = v

instance (Typeable v, Show v) => IsInfo (InfoVal v) where
	propagateInfo _ = False

fromInfoVal :: InfoVal v -> Maybe v
fromInfoVal NoInfoVal = Nothing
fromInfoVal (InfoVal v) = Just v
