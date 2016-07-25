{-# LANGUAGE PackageImports #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Propellor.Types.Core where

import Propellor.Types.Info
import Propellor.Types.OS
import Propellor.Types.Result

import Data.Monoid
import "mtl" Control.Monad.RWS.Strict
import Control.Monad.Catch
import Control.Applicative
import Prelude

-- | Everything Propellor knows about a system: Its hostname,
-- properties and their collected info.
data Host = Host
	{ hostName :: HostName
	, hostProperties :: [ChildProperty]
	, hostInfo :: Info
	}
	deriving (Show, Typeable)

-- | Propellor's monad provides read-only access to info about the host
-- it's running on, and a writer to accumulate EndActions.
newtype Propellor p = Propellor { runWithHost :: RWST Host [EndAction] () IO p }
	deriving
		( Monad
		, Functor
		, Applicative
		, MonadReader Host
		, MonadWriter [EndAction]
		, MonadIO
		, MonadCatch
		, MonadThrow
		, MonadMask
		)

class LiftPropellor m where
	liftPropellor :: m a -> Propellor a

instance LiftPropellor Propellor where
	liftPropellor = id

instance LiftPropellor IO where
	liftPropellor = liftIO

instance Monoid (Propellor Result) where
	mempty = return NoChange
	-- | The second action is only run if the first action does not fail.
	mappend x y = do
		rx <- x
		case rx of
			FailedChange -> return FailedChange
			_ -> do
				ry <- y
				return (rx <> ry)

-- | An action that Propellor runs at the end, after trying to satisfy all
-- properties. It's passed the combined Result of the entire Propellor run.
data EndAction = EndAction Desc (Result -> Propellor Result)

type Desc = String

-- | Props is a combination of a list of properties, with their combined 
-- metatypes.
data Props metatypes = Props [ChildProperty]

-- | Since there are many different types of Properties, they cannot be put
-- into a list. The simplified ChildProperty can be put into a list.
data ChildProperty = ChildProperty Desc (Propellor Result) Info [ChildProperty]
  
instance Show ChildProperty where
	show p = "property " ++ show (getDesc p)

class IsProp p where
	setDesc :: p -> Desc -> p
	getDesc :: p -> Desc
	getChildren :: p -> [ChildProperty]
	addChildren :: p -> [ChildProperty] -> p
	-- | Gets the info of the property, combined with all info
	-- of all children properties.
	getInfoRecursive :: p -> Info
	-- | Info, not including info from children.
	getInfo :: p -> Info
	-- | Gets a ChildProperty representing the Property.
	-- You should not normally need to use this.
	toChildProperty :: p -> ChildProperty
	-- | Gets the action that can be run to satisfy a Property.
	-- You should never run this action directly. Use
	-- 'Propellor.EnsureProperty.ensureProperty` instead.
	getSatisfy :: p -> Propellor Result

instance IsProp ChildProperty where
	setDesc (ChildProperty _ a i c) d = ChildProperty d a i c
	getDesc (ChildProperty d _ _ _) = d
	getChildren (ChildProperty _ _ _ c) = c
	addChildren (ChildProperty d a i c) c' = ChildProperty d a i (c ++ c')
	getInfoRecursive (ChildProperty _ _ i c) =
		i <> mconcat (map getInfoRecursive c)
	getInfo (ChildProperty _ _ i _) = i
	toChildProperty = id
	getSatisfy (ChildProperty _ a _ _) = a
