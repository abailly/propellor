{-# LANGUAGE TypeOperators, PolyKinds, DataKinds, TypeFamilies, UndecidableInstances, FlexibleInstances, GADTs #-}

module Propellor.Types.MetaTypes (
	MetaType(..),
	UnixLike,
	Linux,
	DebianLike,
	Debian,
	Buntish,
	FreeBSD,
	HasInfo,
	MetaTypes,
	type (+),
	sing,
	SingI,
	IncludesInfo,
	Targets,
	NonTargets,
	NotSuperset,
	Combine,
	CheckCombine(..),
	CheckCombinable,
	type (&&),
	Not,
	EqT,
	Union,
) where

import Propellor.Types.Singletons
import Propellor.Types.OS

data MetaType
	= Targeting TargetOS -- ^ A target OS of a Property
	| WithInfo           -- ^ Indicates that a Property has associated Info
	deriving (Show, Eq, Ord)

-- | Any unix-like system
type UnixLike = MetaTypes '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish, 'Targeting 'OSFreeBSD ]
-- | Any linux system
type Linux = MetaTypes '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish ]
-- | Debian and derivatives.
type DebianLike = MetaTypes '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish ]
type Debian = MetaTypes '[ 'Targeting 'OSDebian ]
type Buntish = MetaTypes '[ 'Targeting 'OSBuntish ]
type FreeBSD = MetaTypes '[ 'Targeting 'OSFreeBSD ]

-- | Used to indicate that a Property adds Info to the Host where it's used.
type HasInfo = MetaTypes '[ 'WithInfo ]

type family IncludesInfo t :: Bool
type instance IncludesInfo (MetaTypes l) = Elem 'WithInfo l

type MetaTypes = Sing

-- This boilerplate would not be needed if the singletons library were
-- used. However, we're targeting too old a version of ghc to use it yet.
data instance Sing (x :: MetaType) where
	OSDebianS :: Sing ('Targeting 'OSDebian)
	OSBuntishS :: Sing ('Targeting 'OSBuntish)
	OSFreeBSDS :: Sing ('Targeting 'OSFreeBSD)
	WithInfoS :: Sing 'WithInfo
instance SingI ('Targeting 'OSDebian) where sing = OSDebianS
instance SingI ('Targeting 'OSBuntish) where sing = OSBuntishS
instance SingI ('Targeting 'OSFreeBSD) where sing = OSFreeBSDS
instance SingI 'WithInfo where sing = WithInfoS
instance SingKind ('KProxy :: KProxy MetaType) where
	type DemoteRep ('KProxy :: KProxy MetaType) = MetaType
	fromSing OSDebianS = Targeting OSDebian
	fromSing OSBuntishS = Targeting OSBuntish
	fromSing OSFreeBSDS = Targeting OSFreeBSD
	fromSing WithInfoS = WithInfo

-- | Convenience type operator to combine two `MetaTypes` lists.
--
-- For example:
--
-- > HasInfo + Debian
--
-- Which is shorthand for this type:
--
-- > MetaTypes '[WithInfo, Targeting OSDebian]
type family a + b :: ab
type instance (MetaTypes a) + (MetaTypes b) = MetaTypes (Concat a b)

type family Concat (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Concat '[] bs = bs
type instance Concat (a ': as) bs = a ': (Concat as bs)

-- | Combine two MetaTypes lists, yielding a list
-- that has targets present in both, and nontargets present in either.
type family Combine (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Combine (list1 :: [a]) (list2 :: [a]) =
	(Concat
		(NonTargets list1 `Union` NonTargets list2)
		(Targets list1 `Intersect` Targets list2)
	)

-- | Checks if two MetaTypes lists can be safely combined.
--
-- This should be used anywhere Combine is used, as an additional
-- constraint. For example:
--
-- > foo :: (CheckCombinable x y ~ 'CanCombine) => x -> y -> Combine x y
type family CheckCombinable (list1 :: [a]) (list2 :: [a]) :: CheckCombine
-- As a special case, if either list is empty, let it be combined with the
-- other. This relies on MetaTypes list always containing at least
-- one target, so can only happen if there's already been a type error.
-- This special case lets the type checker show only the original type
-- error, and not an extra error due to a later CheckCombinable constraint.
type instance CheckCombinable '[] list2 = 'CanCombine
type instance CheckCombinable list1 '[] = 'CanCombine
type instance CheckCombinable (l1 ': list1) (l2 ': list2) =
	CheckCombinable' (Combine (l1 ': list1) (l2 ': list2))
type family CheckCombinable' (combinedlist :: [a]) :: CheckCombine
type instance CheckCombinable' '[] = 'CannotCombineTargets
type instance CheckCombinable' (a ': rest) 
	= If (IsTarget a)
		'CanCombine
		(CheckCombinable' rest)

data CheckCombine = CannotCombineTargets | CanCombine

-- | Every item in the subset must be in the superset.
--
-- The name of this was chosen to make type errors more understandable.
type family NotSuperset (superset :: [a]) (subset :: [a]) :: CheckCombine
type instance NotSuperset superset '[] = 'CanCombine
type instance NotSuperset superset (s ': rest) =
	If (Elem s superset)
		(NotSuperset superset rest)
		'CannotCombineTargets

type family IsTarget (a :: t) :: Bool
type instance IsTarget ('Targeting a) = 'True
type instance IsTarget 'WithInfo = 'False

type family Targets (l :: [a]) :: [a]
type instance Targets '[] = '[]
type instance Targets (x ': xs) =
	If (IsTarget x)
		(x ': Targets xs)
		(Targets xs)

type family NonTargets (l :: [a]) :: [a]
type instance NonTargets '[] = '[]
type instance NonTargets (x ': xs) =
	If (IsTarget x)
		(NonTargets xs)
		(x ': NonTargets xs)

-- | Type level elem
type family Elem (a :: t) (list :: [t]) :: Bool
type instance Elem a '[] = 'False
type instance Elem a (b ': bs) = EqT a b || Elem a bs

-- | Type level union.
type family Union (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Union '[] list2 = list2
type instance Union (a ': rest) list2 =
	If (Elem a list2 || Elem a rest)
		(Union rest list2)
		(a ': Union rest list2)

-- | Type level intersection. Duplicate list items are eliminated.
type family Intersect (list1 :: [a]) (list2 :: [a]) :: [a]
type instance Intersect '[] list2 = '[]
type instance Intersect (a ': rest) list2 = 
	If (Elem a list2 && Not (Elem a rest))
		(a ': Intersect rest list2)
		(Intersect rest list2)

-- | Type level equality
--
-- This is a very clumsy implmentation, but it works back to ghc 7.6.
type family EqT (a :: t) (b :: t) :: Bool
type instance EqT ('Targeting a) ('Targeting b)  = EqT a b
type instance EqT 'WithInfo      'WithInfo       = 'True
type instance EqT 'WithInfo      ('Targeting b)  = 'False
type instance EqT ('Targeting a) 'WithInfo       = 'False
type instance EqT 'OSDebian  'OSDebian  = 'True
type instance EqT 'OSBuntish 'OSBuntish = 'True
type instance EqT 'OSFreeBSD 'OSFreeBSD = 'True
type instance EqT 'OSDebian  'OSBuntish = 'False
type instance EqT 'OSDebian  'OSFreeBSD = 'False
type instance EqT 'OSBuntish 'OSDebian  = 'False
type instance EqT 'OSBuntish 'OSFreeBSD = 'False
type instance EqT 'OSFreeBSD 'OSDebian  = 'False
type instance EqT 'OSFreeBSD 'OSBuntish = 'False
-- More modern version if the combinatiorial explosion gets too bad later:
--
-- type family Eq (a :: MetaType) (b :: MetaType) where
-- 	Eq a a = True
-- 	Eq a b = False

-- | An equivilant to the following is in Data.Type.Bool in
-- modern versions of ghc, but is included here to support ghc 7.6.
type family If (cond :: Bool) (tru :: a) (fls :: a) :: a
type instance If 'True  tru fls = tru
type instance If 'False tru fls = fls
type family (a :: Bool) || (b :: Bool) :: Bool
type instance    'False || 'False = 'False
type instance    'True  || 'True  = 'True
type instance    'True  || 'False = 'True
type instance    'False || 'True  = 'True
type family (a :: Bool) && (b :: Bool) :: Bool
type instance    'False && 'False = 'False
type instance    'True  && 'True  = 'True
type instance    'True  && 'False = 'False
type instance    'False && 'True  = 'False
type family Not (a :: Bool) :: Bool
type instance Not 'False = 'True
type instance Not 'True = 'False

