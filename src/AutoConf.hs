{-| Default configuration for Idly.

Note that this file is the default, can be overriden via the cli parameters.

-}


module AutoConf where

split :: String -> [String]
split str =
  case span (/= ',') str of
    (x, []) -> [x]
    (x, _:xs) -> x:split xs

versionMajor :: Int
versionMajor = 0

versionMinor :: Int
versionMinor = 1

localstatedir :: String
localstatedir = "/"


-- Write dictionary with man page name as the key and the section
-- number as the value

pyAfInet4 :: Int
pyAfInet4 = 4

pyAfInet6 :: Int
pyAfInet6 = 6
