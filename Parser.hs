-- parsers for text files
module Parser where
import MapType
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Word
readW :: String -> Word8
readW = read
readP :: String -> Word16
readP = read
parseDef :: String -> DefMap
parseDef = Map.fromList . map ((\[p,r,g,b] -> (readP p,(readW r,readW g,readW b))) . take 4 . splitOn ";") . tail . lines
parseRDef :: String -> ReverseDefMap
parseRDef = Map.fromList . map ((\[p,r,g,b] -> ((readW r,readW g,readW b),readP p)) . take 4 . splitOn ";") . tail . lines
