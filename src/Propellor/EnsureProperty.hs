{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Propellor.EnsureProperty
	( ensureProperty
	, property'
	, OuterMetaTypesWitness(..)
	) where

import Propellor.Types
import Propellor.Types.Core
import Propellor.Types.MetaTypes
import Propellor.Exception

import Data.Monoid
import Prelude

-- | For when code running in the Propellor monad needs to ensure a
-- Property.
--
-- Use `property'` to get the `OuterMetaTypesWithness`. For example:
--
-- > foo = Property Debian
-- > foo = property' "my property" $ \w -> do
-- > 	ensureProperty w (aptInstall "foo")
--
-- The type checker will prevent using ensureProperty with a property
-- that does not support the target OSes needed by the OuterMetaTypesWitness.
-- In the example above, aptInstall must support Debian, since foo
-- is supposed to support Debian.
--
-- The type checker will also prevent using ensureProperty with a property
-- with HasInfo in its MetaTypes. Doing so would cause the `Info` associated
-- with the property to be lost.
ensureProperty
	::
		( Cannot_ensureProperty_WithInfo inner ~ 'True
		, (Targets inner `NotSuperset` Targets outer) ~ 'CanCombine
		)
	=> OuterMetaTypesWitness outer
	-> Property (MetaTypes inner)
	-> Propellor Result
ensureProperty _ = catchPropellor . getSatisfy

-- The name of this was chosen to make type errors a more understandable.
type family Cannot_ensureProperty_WithInfo (l :: [a]) :: Bool
type instance Cannot_ensureProperty_WithInfo '[] = 'True
type instance Cannot_ensureProperty_WithInfo (t ': ts) =
	Not (t `EqT` 'WithInfo) && Cannot_ensureProperty_WithInfo ts

-- | Constructs a property, like `property`, but provides its
-- `OuterMetaTypesWitness`.
property'
	:: SingI metatypes
	=> Desc
	-> (OuterMetaTypesWitness metatypes -> Propellor Result)
	-> Property (MetaTypes metatypes)
property' d a =
	let p = Property sing d (a (outerMetaTypesWitness p)) mempty mempty
	in p

-- | Used to provide the metatypes of a Property to calls to 
-- 'ensureProperty` within it.
newtype OuterMetaTypesWitness metatypes = OuterMetaTypesWitness (MetaTypes metatypes)

outerMetaTypesWitness :: Property (MetaTypes l) -> OuterMetaTypesWitness l
outerMetaTypesWitness (Property metatypes _ _ _ _) = OuterMetaTypesWitness metatypes
