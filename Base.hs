{-# LANGUAGE DataKinds #-}

module Base where

import Propellor.Types (TargetOS (..))
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)

type OS = MetaTypes '[ 'WithInfo, 'Targeting 'OSDebian, 'Targeting 'OSBuntish]
