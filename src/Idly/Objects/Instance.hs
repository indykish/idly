{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

{-| Implementation of the Idly Instance config object.

-}

module Idly.Objects.Instance where

import           Data.Monoid

import           Idly.JSON        (emptyContainer)
import           Idly.Objects.Nic
import           Idly.THH
import           Idly.THH.Field
import           Idly.Types
import           Idly.Utils       (parseUnitAssumeBinary)

$(buildParam "Be" "bep"
  [ specialNumericalField 'parseUnitAssumeBinary
      $ simpleField "minmem"      [t| Int  |]
  , specialNumericalField 'parseUnitAssumeBinary
      $ simpleField "maxmem"      [t| Int  |]
  , simpleField "vcpus"           [t| Int  |]
  , simpleField "auto_balance"    [t| Bool |]
  , simpleField "always_failover" [t| Bool |]
  , simpleField "spindle_use"     [t| Int  |]
  ])

$(buildObjectWithForthcoming "Instance" "inst" $
  [ simpleField "name"             [t| String             |]
  , simpleField "primary_node"     [t| String             |]
  , simpleField "os"               [t| String             |]
  , simpleField "hypervisor"       [t| Hypervisor         |]
  , defaultField [| emptyContainer |]
      $ simpleField "hvparams"     [t| HvParams           |]
  , defaultField [| mempty |]
      $ simpleField "beparams"     [t| PartialBeParams    |]
  , defaultField [| emptyContainer |]
      $ simpleField "osparams"     [t| OsParams           |]
  , defaultField [| emptyContainer |]
      $ simpleField "osparams_private" [t| OsParamsPrivate |]
  , simpleField "admin_state"      [t| AdminState         |]
  , simpleField "admin_state_source" [t| AdminStateSource   |]
  , defaultField [| [] |]
      $ simpleField "nics"         [t| [PartialNic]       |]
  , defaultField [| [] |]
      $ simpleField "disks"        [t| [String]           |]
  , simpleField "disks_active"     [t| Bool               |]
  , optionalField $ simpleField "network_port" [t| Int  |]
  ]
  ++ timeStampFields
  ++ uuidFields
  ++ serialFields
  ++ tagsFields)

instance TimeStampObject Instance where
  cTimeOf = instCtime
  mTimeOf = instMtime

instance UuidObject Instance where
  uuidOf = instUuid

instance SerialNoObject Instance where
  serialOf = instSerial

instance TagsObject Instance where
  tagsOf = instTags

instance ForthcomingObject Instance where
  isForthcoming = instForthcoming
