-- parsers for text files
module Parser where
import MapType
import HistoryType
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Data.Char
import Text.Regex.Posix
readW :: String -> Word8
readW = read
readP :: String -> Word16
readP = read

parseDef :: String -> DefMap
parseDef = Map.fromList . map ((\[p,r,g,b] -> (readP p,(readW r,readW g,readW b))) . take 4 . splitOn ";") . tail . lines
parseRDef :: String -> ReverseDefMap
parseRDef = Map.fromList . map ((\[p,r,g,b] -> ((readW r,readW g,readW b),readP p)) . take 4 . splitOn ";") . tail . lines

removeRedundant :: [String] -> [String]
removeRedundant ss = [x | x<- ss, x/="", head x/='#']
-- get attr from a line of descriptoin like in "owner = MNG" or "base_production = 2"
captureAttr :: String -> (String, String, String, [String])
captureAttr = (=~ "^([a-zA-Z0-9_]+)[ \t]*=[ \t]*([a-zA-Z0-9_]+)")
parseHistory :: String -> Map.Map String String
parseHistory = Map.fromList . map (\[a,b] -> (a,b)) . filter (/=[]) . map ((\(_,_,_,d) -> d) . captureAttr) . removeRedundant . lines
buildCountry :: String -> [[String]] -> Country
buildCountry = undefined
buildProvince :: Word16 -> Map.Map String String -> ProvinceHistory
buildProvince pid attrMap = PHistory pid man tax pro ishre [(owner,(1444,11,11))] where
  getAttr attr = fromMaybe "" $ Map.lookup attr attrMap
  vals = map getAttr ["base_manpower","base_tax","base_production"]
  -- if manpower / tax / production nonzero, but no controller -> lands to be colonized
  man = if head vals /= "" then readW (head vals) else 0
  tax = if head vals /= "" then readW (vals !! 1) else 0
  pro = if head vals /= "" then readW (last vals) else 0
  ishre = getAttr "hre" == "yes"
  -- if no controller and manpower / tax / production zero then waste land
  owner = getAttr "controller"

isProvHistory :: FilePath -> Bool
isProvHistory = (=~ "^[0-9]+")
-- assuming already is prov history file
fpToProv :: FilePath -> (Word16, String)
fpToProv fp = (pid, name) where
  pid = readP $ fp =~ "^[0-9]+"
  name = fp =~ "[a-zA-Z][-a-zA-Z ]+"

strToColor :: String -> (String, String, String, [String])
strToColor = (=~ "([0-9]+)[ \t]+([0-9]+)[ \t]+([0-9]+)")
parseColor :: String -> [Word8]
parseColor = map readW . (\(_,_,_,a) -> a) . strToColor . (=~ "[0-9]+[ \t]+[0-9]+[ \t]+[0-9]+")
-- hash three letter country code into Word16 int
hashC :: String -> Int
hashC cs = sum $ zipWith (\a b -> (ord a - ord 'A') * b) cs [1,26,676]

isCountry :: FilePath -> Bool
isCountry = (=~ "[A-Z][a-z][a-zA-Z]+")
isLocal :: String -> Bool
isLocal = (=~ " [A-Z]{3}:")
oneLocal :: String -> (Word16, String)
oneLocal d = (cid, name) where
  cid = fromIntegral . hashC $ d =~ "[A-Z]{3}"
  name = d =~ "[A-Z][a-z][a-zA-Z ]+"
-- [snd x | x<- a, length [y | y<- fs, (map toLower (snd x)) `isPrefixOf` (map toLower y)] == 0 ]

isPathConfig :: String -> Bool
isPathConfig = (=~ "[A-Z]{3}")
getPathConfig :: String -> (String, String, String, [String])
getPathConfig = (=~ "([A-Z]{3})[\t ]*= \"([ ./_a-zA-Z]+)\"")
