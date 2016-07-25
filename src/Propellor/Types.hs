{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Propellor.Types (
	-- * Core data types
	  Host(..)
	, Property(..)
	, property
	, Desc
	, RevertableProperty(..)
	, (<!>)
	, Propellor(..)
	, LiftPropellor(..)
	, Info
	-- * Types of properties
	, UnixLike
	, Linux
	, DebianLike
	, Debian
	, Buntish
	, FreeBSD
	, HasInfo
	, type (+)
	, TightenTargets(..)
	-- * Combining and modifying properties
	, Combines(..)
	, CombinedType
	, ResultCombiner
	, adjustPropertySatisfy
	-- * Other included types
	, module Propellor.Types.OS
	, module Propellor.Types.Dns
	, module Propellor.Types.Result
	, module Propellor.Types.ZFS
	) where

import Data.Monoid

import Propellor.Types.Core
import Propellor.Types.Info
import Propellor.Types.OS
import Propellor.Types.Dns
import Propellor.Types.Result
import Propellor.Types.MetaTypes
import Propellor.Types.ZFS

-- | The core data type of Propellor, this represents a property
-- that the system should have, with a descrition, and an action to ensure
-- it has the property.
-- that have the property.
--
-- There are different types of properties that target different OS's,
-- and so have different metatypes. 
-- For example: "Property DebianLike" and "Property FreeBSD".
--
-- Also, some properties have associated `Info`, which is indicated in
-- their type: "Property (HasInfo + DebianLike)"
--
-- There are many associated type families, which are mostly used
-- internally, so you needn't worry about them.
data Property metatypes = Property metatypes Desc (Propellor Result) Info [ChildProperty]

instance Show (Property metatypes) where
	show p = "property " ++ show (getDesc p)

-- | Constructs a Property, from a description and an action to run to
-- ensure the Property is met.
--
-- Due to the polymorphic return type of this function, most uses will need
-- to specify a type signature. This lets you specify what OS the property
-- targets, etc.
--
-- For example:
--
-- > foo :: Property Debian
-- > foo = property "foo" $ do
-- >	...
-- > 	return MadeChange
property
	:: SingI metatypes
	=> Desc
	-> Propellor Result
	-> Property (MetaTypes metatypes)
property d a = Property sing d a mempty mempty

-- | Changes the action that is performed to satisfy a property.
adjustPropertySatisfy :: Property metatypes -> (Propellor Result -> Propellor Result) -> Property metatypes
adjustPropertySatisfy (Property t d s i c) f = Property t d (f s) i c

-- | A property that can be reverted. The first Property is run
-- normally and the second is run when it's reverted.
data RevertableProperty setupmetatypes undometatypes = RevertableProperty
	{ setupRevertableProperty :: Property setupmetatypes
	, undoRevertableProperty :: Property undometatypes
	}

instance Show (RevertableProperty setupmetatypes undometatypes) where
	show (RevertableProperty p _) = show p

-- | Shorthand to construct a revertable property from any two Properties.
(<!>)
	:: Property setupmetatypes
	-> Property undometatypes
	-> RevertableProperty setupmetatypes undometatypes
setup <!> undo = RevertableProperty setup undo

instance IsProp (Property metatypes) where
	setDesc (Property t _ a i c) d = Property t d a i c
	getDesc (Property _ d _ _ _) = d
	getChildren (Property _ _ _ _ c) = c
	addChildren (Property t d a i c) c' = Property t d a i (c ++ c')
	getInfoRecursive (Property _ _ _ i c) =
		i <> mconcat (map getInfoRecursive c)
	getInfo (Property _ _ _ i _) = i
	toChildProperty (Property _ d a i c) = ChildProperty d a i c
	getSatisfy (Property _ _ a _ _) = a

instance IsProp (RevertableProperty setupmetatypes undometatypes) where
	-- | Sets the description of both sides.
	setDesc (RevertableProperty p1 p2) d =
		RevertableProperty (setDesc p1 d) (setDesc p2 ("not " ++ d))
	getDesc (RevertableProperty p1 _) = getDesc p1
	getChildren (RevertableProperty p1 _) = getChildren p1
	-- | Only add children to the active side.
	addChildren (RevertableProperty p1 p2) c = RevertableProperty (addChildren p1 c) p2
	-- | Return the Info of the currently active side.
	getInfoRecursive (RevertableProperty p1 _p2) = getInfoRecursive p1
	getInfo (RevertableProperty p1 _p2) = getInfo p1
	toChildProperty (RevertableProperty p1 _p2) = toChildProperty p1
	getSatisfy (RevertableProperty p1 _) = getSatisfy p1

-- | Type level calculation of the type that results from combining two
-- types of properties.
type family CombinedType x y
type instance CombinedType (Property (MetaTypes x)) (Property (MetaTypes y)) = Property (MetaTypes (Combine x y))
type instance CombinedType (RevertableProperty (MetaTypes x) (MetaTypes x')) (RevertableProperty (MetaTypes y) (MetaTypes y')) = RevertableProperty (MetaTypes (Combine x y)) (MetaTypes (Combine x' y'))
-- When only one of the properties is revertable, the combined property is
-- not fully revertable, so is not a RevertableProperty.
type instance CombinedType (RevertableProperty (MetaTypes x) (MetaTypes x')) (Property (MetaTypes y)) = Property (MetaTypes (Combine x y))
type instance CombinedType (Property (MetaTypes x)) (RevertableProperty (MetaTypes y) (MetaTypes y')) = Property (MetaTypes (Combine x y))

type ResultCombiner = Propellor Result -> Propellor Result -> Propellor Result

class Combines x y where
	-- | Combines together two properties, yielding a property that
	-- has the description and info of the first, and that has the
	-- second property as a child property.
	combineWith
		:: ResultCombiner
		-- ^ How to combine the actions to satisfy the properties.
		-> ResultCombiner
		-- ^ Used when combining revertable properties, to combine
		-- their reversion actions.
		-> x
		-> y
		-> CombinedType x y

instance (CheckCombinable x y ~ 'CanCombine, SingI (Combine x y)) => Combines (Property (MetaTypes x)) (Property (MetaTypes y)) where
	combineWith f _ (Property _ d1 a1 i1 c1) (Property _ d2 a2 i2 c2) =
		Property sing d1 (f a1 a2) i1 (ChildProperty d2 a2 i2 c2 : c1)
instance (CheckCombinable x y ~ 'CanCombine, CheckCombinable x' y' ~ 'CanCombine, SingI (Combine x y), SingI (Combine x' y')) => Combines (RevertableProperty (MetaTypes x) (MetaTypes x')) (RevertableProperty (MetaTypes y) (MetaTypes y')) where
	combineWith sf tf (RevertableProperty s1 t1) (RevertableProperty s2 t2) =
		RevertableProperty
			(combineWith sf tf s1 s2)
			(combineWith tf sf t1 t2)
instance (CheckCombinable x y ~ 'CanCombine, SingI (Combine x y)) => Combines (RevertableProperty (MetaTypes x) (MetaTypes x')) (Property (MetaTypes y)) where
	combineWith sf tf (RevertableProperty x _) y = combineWith sf tf x y
instance (CheckCombinable x y ~ 'CanCombine, SingI (Combine x y)) => Combines (Property (MetaTypes x)) (RevertableProperty (MetaTypes y) (MetaTypes y')) where
	combineWith sf tf x (RevertableProperty y _) = combineWith sf tf x y

class TightenTargets p where
	-- | Tightens the MetaType list of a Property (or similar),
	-- to contain fewer targets.
	--
	-- For example, to make a property that uses apt-get, which is only
	-- available on DebianLike systems:
	--
	-- > upgraded :: Property DebianLike
	-- > upgraded = tightenTargets $ cmdProperty "apt-get" ["upgrade"]
	tightenTargets
		:: 
			-- Note that this uses PolyKinds
			( (Targets untightened `NotSuperset` Targets tightened) ~ 'CanCombine
			, (NonTargets tightened `NotSuperset` NonTargets untightened) ~ 'CanCombine
			, SingI tightened
			)
		=> p (MetaTypes untightened)
		-> p (MetaTypes tightened)

instance TightenTargets Property where
	tightenTargets (Property _ d a i c) = Property sing d a i c
