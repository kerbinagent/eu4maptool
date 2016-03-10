module HistoryType where
import Data.Word
import qualified Data.Map.Strict as Map
type HistoryDate = (Word16,Word8,Word8)
data Monarch = Monarch {
monarchname::String,
birthdate::HistoryDate,
deathdate::HistoryDate,
getAdm::Word8,
getDip::Word8,
getMil::Word8
}

data Country = Country {
countryID::Word16,
countryname::String,
getcolor::[Word8]
}
  deriving(Show)

data ProvinceHistory = PHistory {
provinceID::Word16,
baseManpower :: Word8,
baseTax :: Word8,
baseProduction :: Word8,
-- provinceReligion::Int,
hre::Bool,
controllers::[(String,HistoryDate)]
}
  deriving(Show)

type ProvCountryMap = Map.Map Word16 Word16
type CountryMap = Map.Map Word16 Country
