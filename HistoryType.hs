module HistoryType where
import Data.Word
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
countryname::String,
religion::Int,
capital::Word16,
leaders::[Monarch]
}

data ProvinceHistory = PHistory {
provinceID::Int,
baseManpower :: Word8,
baseTax :: Word8,
baseProduction :: Word8,
-- provinceReligion::Int,
hre::Bool,
controllers::[(String,HistoryDate)]
}
  deriving(Show)
