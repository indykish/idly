{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

{-| Implementation of the Idly config objects.

-}

module Idly.Objects
  ( HvParams
  , OsParams
  , OsParamsPrivate
  , PartialNicParams(..)
  , FilledNicParams(..)
  , allNicParamFields
  , PartialNic(..)
  , FileDriver(..)
  , DataCollectorConfig(..)
  , DiskTemplate(..)
  , PartialBeParams(..)
  , FilledBeParams(..)
  , PartialNDParams(..)
  , FilledNDParams(..)
  , allNDParamFields
  , Node(..)
  , AllocPolicy(..)
  , GroupDiskParams
  , NodeGroup(..)
  , FilterAction(..)
  , IpFamily(..)
  , ipFamilyToRaw
  , fillDict
  , UidPool
  , formatUidRange
  , UidRange
  , Cluster(..)
  , ConfigData(..)
  , TimeStampObject(..) -- re-exported from Types
  , UuidObject(..) -- re-exported from Types
  , SerialNoObject(..) -- re-exported from Types
  , TagsObject(..) -- re-exported from Types
  , DictObject(..) -- re-exported from THH
  , TagSet -- re-exported from THH
  , Network(..)
  , AddressPool(..)
  , Ip4Address()
  , mkIp4Address
  , Ip4Network()
  , mkIp4Network
  , ip4netAddr
  , ip4netMask
  , readIp4Address
  , ip4AddressToList
  , ip4AddressToNumber
  , ip4AddressFromNumber
  , nextIp4Address
  , IAllocatorParams
  , MasterNetworkParameters(..)
  , module Idly.PartialParams
  , module Idly.Objects.Disk
  , module Idly.Objects.Instance
  ) where

import           Control.Applicative   ()
import           Control.Arrow         (first)
import           Control.Monad.State
import           Data.List             (foldl', intercalate)
import qualified Data.Map              as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord              (comparing)
import           Data.Ratio            (denominator, numerator)
import           Data.Tuple            (swap)
import           Data.Word
import           Text.JSON             (JSON, JSValue (..), fromJSString,
                                        readJSON, showJSON, toJSString)
import qualified Text.JSON             as J

import qualified AutoConf
import qualified Idly.Constants        as C
import qualified Idly.ConstantUtils    as ConstantUtils
import           Idly.JSON
import           Idly.Objects.BitArray (BitArray)
import           Idly.Objects.Disk
import           Idly.Objects.Instance
import           Idly.Objects.Nic
import           Idly.PartialParams
import           Idly.THH
import           Idly.THH.Field
import           Idly.Types
import           Idly.Utils            (sepSplit, tryRead)

-- * Generic definitions

-- | Fills one map with keys from the other map, if not already
-- existing. Mirrors objects.py:FillDict.
fillDict :: (Ord k) => Map.Map k v -> Map.Map k v -> [k] -> Map.Map k v
fillDict defaults custom skip_keys =
  let updated = Map.union custom defaults
  in foldl' (flip Map.delete) updated skip_keys


-- * Network definitions

-- ** Ipv4 types

data Ip4Address = Ip4Address Word8 Word8 Word8 Word8
  deriving (Eq, Ord)

mkIp4Address :: (Word8, Word8, Word8, Word8) -> Ip4Address
mkIp4Address (a, b, c, d) = Ip4Address a b c d

instance Show Ip4Address where
  show (Ip4Address a b c d) = intercalate "." $ map show [a, b, c, d]

readIp4Address :: (Applicative m, Monad m) => String -> m Ip4Address
readIp4Address s =
  case sepSplit '.' s of
    [a, b, c, d] -> Ip4Address <$>
                      tryRead "first octect" a <*>
                      tryRead "second octet" b <*>
                      tryRead "third octet"  c <*>
                      tryRead "fourth octet" d
    _ -> fail $ "Can't parse IPv4 address from string " ++ s

instance JSON Ip4Address where
  showJSON = showJSON . show
  readJSON (JSString s) = readIp4Address (fromJSString s)
  readJSON v = fail $ "Invalid JSON value " ++ show v ++ " for an IPv4 address"

-- Converts an address to a list of numbers
ip4AddressToList :: Ip4Address -> [Word8]
ip4AddressToList (Ip4Address a b c d) = [a, b, c, d]

-- | Converts an address into its ordinal number.
-- This is needed for indexing IP adresses in reservation pools.
ip4AddressToNumber :: Ip4Address -> Integer
ip4AddressToNumber = foldl (\n i -> 256 * n + toInteger i) 0 . ip4AddressToList

-- | Converts a number into an address.
-- This is needed for indexing IP adresses in reservation pools.
ip4AddressFromNumber :: Integer -> Ip4Address
ip4AddressFromNumber n =
  let s = state $ first fromInteger . swap . (`divMod` 256)
      (d, c, b, a) = evalState ((,,,) <$> s <*> s <*> s <*> s) n
   in Ip4Address a b c d

nextIp4Address :: Ip4Address -> Ip4Address
nextIp4Address = ip4AddressFromNumber . (+ 1) . ip4AddressToNumber

-- | Custom type for an IPv4 network.
data Ip4Network = Ip4Network { ip4netAddr :: Ip4Address
                             , ip4netMask :: Word8
                             }
                  deriving (Eq)

mkIp4Network :: Ip4Address -> Word8 -> Ip4Network
mkIp4Network = Ip4Network

instance Show Ip4Network where
  show (Ip4Network ip netmask) = show ip ++ "/" ++ show netmask

-- | JSON instance for 'Ip4Network'.
instance JSON Ip4Network where
  showJSON = showJSON . show
  readJSON (JSString s) =
    case sepSplit '/' (fromJSString s) of
      [ip, nm] -> do
        ip' <- readIp4Address ip
        nm' <- tryRead "parsing netmask" nm
        if nm' >= 0 && nm' <= 32
          then return $ Ip4Network ip' nm'
          else fail $ "Invalid netmask " ++ show nm' ++ " from string " ++
                      fromJSString s
      _ -> fail $ "Can't parse IPv4 network from string " ++ fromJSString s
  readJSON v = fail $ "Invalid JSON value " ++ show v ++ " for an IPv4 network"

-- ** Address pools

-- | Currently address pools just wrap a reservation 'BitArray'.
--
-- In future, 'Network' might be extended to include several address pools
-- and address pools might include their own ranges of addresses.
newtype AddressPool = AddressPool { apReservations :: BitArray }
  deriving (Eq, Ord, Show)

instance JSON AddressPool where
  showJSON = showJSON . apReservations
  readJSON = liftM AddressPool . readJSON

-- ** Idly \"network\" config object.

-- FIXME: Not all types might be correct here, since they
-- haven't been exhaustively deduced from the python code yet.
--
-- FIXME: When parsing, check that the ext_reservations and reservations
-- have the same length
$(buildObject "Network" "network" $
  [ simpleField "name"             [t| NonEmptyString |]
  , optionalField $
    simpleField "mac_prefix"       [t| String |]
  , simpleField "network"          [t| Ip4Network |]
  , optionalField $
    simpleField "network6"         [t| String |]
  , optionalField $
    simpleField "gateway"          [t| Ip4Address |]
  , optionalField $
    simpleField "gateway6"         [t| String |]
  , optionalField $
    simpleField "reservations"     [t| AddressPool |]
  , optionalField $
    simpleField "ext_reservations" [t| AddressPool |]
  ]
  ++ uuidFields
  ++ timeStampFields
  ++ serialFields
  ++ tagsFields)

instance SerialNoObject Network where
  serialOf = networkSerial

instance TagsObject Network where
  tagsOf = networkTags

instance UuidObject Network where
  uuidOf = networkUuid

instance TimeStampObject Network where
  cTimeOf = networkCtime
  mTimeOf = networkMtime


-- * Datacollector definitions
type MicroSeconds = Integer

-- | The configuration regarding a single data collector.
$(buildObject "DataCollectorConfig" "dataCollector" [
  simpleField "active" [t| Bool|],
  simpleField "interval" [t| MicroSeconds |]
  ])


-- * Node definitions

$(buildParam "ND" "ndp"
  [ simpleField "oob_program"   [t| String |]
  , simpleField "spindle_count" [t| Int    |]
  , simpleField "exclusive_storage" [t| Bool |]
  , simpleField "ovs"           [t| Bool |]
  , simpleField "ovs_name"       [t| String |]
  , simpleField "ovs_link"       [t| String |]
  , simpleField "ssh_port"      [t| Int |]
  , simpleField "cpu_speed"     [t| Double |]
  ])

$(buildObject "Node" "node" $
  [ simpleField "name"             [t| String |]
  , simpleField "primary_ip"       [t| String |]
  , simpleField "secondary_ip"     [t| String |]
  , simpleField "master_candidate" [t| Bool   |]
  , simpleField "offline"          [t| Bool   |]
  , simpleField "drained"          [t| Bool   |]
  , simpleField "group"            [t| String |]
  , simpleField "master_capable"   [t| Bool   |]
  , simpleField "vm_capable"       [t| Bool   |]
  , simpleField "ndparams"         [t| PartialNDParams |]
  , simpleField "powered"          [t| Bool   |]
  ]
  ++ timeStampFields
  ++ uuidFields
  ++ serialFields
  ++ tagsFields)

instance TimeStampObject Node where
  cTimeOf = nodeCtime
  mTimeOf = nodeMtime

instance UuidObject Node where
  uuidOf = nodeUuid

instance SerialNoObject Node where
  serialOf = nodeSerial

instance TagsObject Node where
  tagsOf = nodeTags

-- * NodeGroup definitions

-- | The cluster/group disk parameters type.
type GroupDiskParams = Container DiskParams

-- | A mapping from network UUIDs to nic params of the networks.
type Networks = Container PartialNicParams

$(buildObject "NodeGroup" "group" $
  [ simpleField "name"         [t| String |]
  , defaultField [| [] |] $ simpleField "members" [t| [String] |]
  , simpleField "ndparams"     [t| PartialNDParams |]
  , simpleField "alloc_policy" [t| AllocPolicy     |]
  , simpleField "ipolicy"      [t| AllocPolicy  |]
  , simpleField "diskparams"   [t| GroupDiskParams |]
  , simpleField "networks"     [t| Networks        |]
  ]
  ++ timeStampFields
  ++ uuidFields
  ++ serialFields
  ++ tagsFields)

instance TimeStampObject NodeGroup where
  cTimeOf = groupCtime
  mTimeOf = groupMtime

instance UuidObject NodeGroup where
  uuidOf = groupUuid

instance SerialNoObject NodeGroup where
  serialOf = groupSerial

instance TagsObject NodeGroup where
  tagsOf = groupTags

-- * Job scheduler filtering definitions

-- | Actions that can be performed when a filter matches.
data FilterAction
  = Accept
  | Pause
  | Reject
  | Continue
  | RateLimit Int
  deriving (Eq, Ord, Show)

instance JSON FilterAction where
  showJSON fa = case fa of
    Accept      -> JSString (toJSString "ACCEPT")
    Pause       -> JSString (toJSString "PAUSE")
    Reject      -> JSString (toJSString "REJECT")
    Continue    -> JSString (toJSString "CONTINUE")
    RateLimit n -> JSArray [ JSString (toJSString "RATE_LIMIT")
                           , JSRational False (fromIntegral n)
                           ]
  readJSON v = case v of
    -- `FilterAction`s are case-sensitive.
    JSString s | fromJSString s == "ACCEPT"   -> return Accept
    JSString s | fromJSString s == "PAUSE"    -> return Pause
    JSString s | fromJSString s == "REJECT"   -> return Reject
    JSString s | fromJSString s == "CONTINUE" -> return Continue
    JSArray (JSString s : rest) | fromJSString s == "RATE_LIMIT" ->
      case rest of
        [JSRational False n] | denominator n == 1 && numerator n > 0 ->
          return . RateLimit . fromIntegral $ numerator n
        _ -> fail "RATE_LIMIT argument must be a positive integer"
    x -> fail $ "malformed FilterAction JSON: " ++ J.showJSValue x ""


-- | IP family type
$(declareIADT "IpFamily"
  [ ("IpFamilyV4", 'AutoConf.pyAfInet4)
  , ("IpFamilyV6", 'AutoConf.pyAfInet6)
  ])
$(makeJSONInstance ''IpFamily)


-- | A low-high UID ranges.
type UidRange = (Int, Int)

formatUidRange :: UidRange -> String
formatUidRange (lower, higher)
  | lower == higher = show lower
  | otherwise       = show lower ++ "-" ++ show higher

-- | Cluster UID Pool, list (low, high) UID ranges.
type UidPool = [UidRange]

-- | The iallocator parameters type.
type IAllocatorParams = Container JSValue

-- | The master candidate client certificate digests
type CandidateCertificates = Container String

-- | Disk state parameters.
--
-- As according to the documentation this option is unused by Idly,
-- the content is just a 'JSValue'.
type DiskState = Container JSValue

-- | Hypervisor state parameters.
--
-- As according to the documentation this option is unused by Idly,
-- the content is just a 'JSValue'.
type HypervisorState = Container JSValue

-- * Cluster definitions
$(buildObject "Cluster" "cluster" $
  [ simpleField "rsahostkeypub"                  [t| String                  |]
  , optionalField $
    simpleField "dsahostkeypub"                  [t| String                  |]
  , simpleField "highest_used_port"              [t| Int                     |]
  , simpleField "tcpudp_port_pool"               [t| [Int]                   |]
  , simpleField "mac_prefix"                     [t| String                  |]
  , optionalField $
    simpleField "volume_group_name"              [t| String                  |]
  , simpleField "reserved_lvs"                   [t| [String]                |]
  , optionalField $
    simpleField "drbd_usermode_helper"           [t| String                  |]
  , simpleField "master_node"                    [t| String                  |]
  , simpleField "master_ip"                      [t| String                  |]
  , simpleField "master_netdev"                  [t| String                  |]
  , simpleField "master_netmask"                 [t| Int                     |]
  , simpleField "use_external_mip_script"        [t| Bool                    |]
  , simpleField "cluster_name"                   [t| String                  |]
  , simpleField "file_storage_dir"               [t| String                  |]
  , simpleField "shared_file_storage_dir"        [t| String                  |]
  , simpleField "gluster_storage_dir"            [t| String                  |]
  , simpleField "enabled_hypervisors"            [t| [Hypervisor]            |]
  , simpleField "ndparams"                       [t| FilledNDParams          |]
  , simpleField "diskparams"                     [t| GroupDiskParams         |]
  , simpleField "candidate_pool_size"            [t| Int                     |]
  , simpleField "modify_etc_hosts"               [t| Bool                    |]
  , simpleField "modify_ssh_setup"               [t| Bool                    |]
  , simpleField "maintain_node_health"           [t| Bool                    |]
  , simpleField "uid_pool"                       [t| UidPool                 |]
  , simpleField "default_iallocator"             [t| String                  |]
  , simpleField "default_iallocator_params"      [t| IAllocatorParams        |]
  , simpleField "hidden_os"                      [t| [String]                |]
  , simpleField "blacklisted_os"                 [t| [String]                |]
  , simpleField "primary_ip_family"              [t| IpFamily                |]
  , simpleField "prealloc_wipe_disks"            [t| Bool                    |]
  , defaultField [| emptyContainer |] $
    simpleField "hv_state_static"                [t| HypervisorState        |]
  , defaultField [| emptyContainer |] $
    simpleField "disk_state_static"              [t| DiskState              |]
  , simpleField "enabled_disk_templates"         [t| [DiskTemplate]          |]
  , simpleField "candidate_certs"                [t| CandidateCertificates   |]
  , simpleField "max_running_jobs"               [t| Int                     |]
  , simpleField "max_tracked_jobs"               [t| Int                     |]
  , simpleField "install_image"                  [t| String                  |]
  , simpleField "instance_communication_network" [t| String                  |]
  , simpleField "zeroing_image"                  [t| String                  |]
  , simpleField "compression_tools"              [t| [String]                |]
  , simpleField "enabled_user_shutdown"          [t| Bool                    |]
  , simpleField "data_collectors"         [t| Container DataCollectorConfig  |]
 ]
 ++ timeStampFields
 ++ uuidFields
 ++ serialFields
 ++ tagsFields)

instance TimeStampObject Cluster where
  cTimeOf = clusterCtime
  mTimeOf = clusterMtime

instance UuidObject Cluster where
  uuidOf = clusterUuid

instance SerialNoObject Cluster where
  serialOf = clusterSerial

instance TagsObject Cluster where
  tagsOf = clusterTags

-- * ConfigData definitions

$(buildObject "ConfigData" "config" $
--  timeStampFields ++
  [ simpleField "version"    [t| Int                 |]
  , simpleField "cluster"    [t| Cluster             |]
  , simpleField "nodes"      [t| Container Node      |]
  , simpleField "nodegroups" [t| Container NodeGroup |]
  , simpleField "instances"  [t| Container Instance  |]
  , simpleField "networks"   [t| Container Network   |]
  , simpleField "disks"      [t| Container Disk      |]
  ]
  ++ timeStampFields
  ++ serialFields)

instance SerialNoObject ConfigData where
  serialOf = configSerial

instance TimeStampObject ConfigData where
  cTimeOf = configCtime
  mTimeOf = configMtime

-- * Master network parameters

$(buildObject "MasterNetworkParameters" "masterNetworkParameters"
  [ simpleField "uuid"      [t| String   |]
  , simpleField "ip"        [t| String   |]
  , simpleField "netmask"   [t| Int      |]
  , simpleField "netdev"    [t| String   |]
  , simpleField "ip_family" [t| IpFamily |]
  ])
