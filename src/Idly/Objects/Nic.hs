{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

{-| Implementation of the Idly Instance config object.

-}

module Idly.Objects.Nic where

import           Idly.THH
import           Idly.THH.Field
import           Idly.Types

$(buildParam "Nic" "nicp"
  [ simpleField "mode" [t| NICMode |]
  , simpleField "link" [t| String  |]
  , simpleField "vlan" [t| String |]
  ])

$(buildObject "PartialNic" "nic" $
  [ simpleField "mac" [t| String |]
  , optionalField $ simpleField "ip" [t| String |]
  , simpleField "nicparams" [t| PartialNicParams |]
  , optionalField $ simpleField "network" [t| String |]
  , optionalField $ simpleField "name" [t| String |]
  ] ++ uuidFields)

instance UuidObject PartialNic where
  uuidOf = nicUuid
