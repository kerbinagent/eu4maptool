-- parsers for text files
module Parser where
import MapType
import HistoryType
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
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
buildProvince :: Int -> Map.Map String String -> ProvinceHistory
buildProvince pid attrMap = PHistory pid man tax pro ishre [(owner,(1444,11,11))] where
  getAttr attr = fromMaybe "" $ Map.lookup attr attrMap
  man = readW $ getAttr "base_manpower"
  tax = readW $ getAttr "base_tax"
  pro = readW $ getAttr "base_production"
  ishre = getAttr "hre" == "yes"
  owner = getAttr "controller"
