{-# LANGUAGE OverloadedLabels #-}
module Match where

import Control.Newtype.Generics
import GHC.Generics (Generic)
import Data.Text (Text)

type a :* b = (a, b)

newtype Cost = Cost Int
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num)
  deriving anyclass (Newtype)

data TentDimensions = TentDimensions Int Int Int
    deriving stock (Eq, Ord, Show, Generic)

type R = Double

newtype Kg = Kg Int
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num)  
  deriving anyclass (Newtype)

newtype RationItemName = RationItemName Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype RationItemId = RationItemId Int
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Bounded, Enum)
  deriving anyclass (Newtype)

newtype MedicineName = MedicineName Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype MedicineId = MedicineId Int
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Bounded, Enum)
  deriving anyclass (Newtype)

data RationItem = RationItem
  { rationItemId :: RationItemId 
  , rationItemName :: RationItemName
  , rationItemQuantity :: Kg
  , rationItemCost :: Cost
  }
  deriving stock (Eq, Show, Ord, Generic)

data Medicine = Medicine
  { medicineId :: MedicineId
  , medicineName :: MedicineName
  , medicineQuantityPerPack :: Int
  , medicineCostPerPack :: Cost
  }
  deriving stock (Eq, Show, Ord, Generic)

data Gender = Male | Female | Other
  deriving stock (Eq, Show, Ord, Generic, Bounded, Enum)

data Civ = Mashriq | Maghrib
  deriving stock (Eq, Show, Ord, Generic, Bounded, Enum)

data Clothes = Clothes
  { gendered :: Gender
  , ageAppropriate :: Int
  , civilisation :: Civ
  }
  deriving stock (Eq, Show, Generic, Ord)

class HasCost a where
  cost :: a -> Cost

instance HasCost RationItem where
  cost = rationItemCost

instance HasCost Medicine where
  cost = medicineCostPerPack

instance HasCost AidItem where
  cost (Tent _ c) = c
  cost (RationPack xs) = sum (cost <$> xs)
  cost (Medicines xs) = sum (cost <$> xs)

data AidItem where
  Tent :: TentDimensions -> Cost -> AidItem
  RationPack :: [RationItem] -> AidItem
  Medicines :: [Medicine] -> AidItem
  deriving stock (Eq, Show, Generic)

data Province = Balochistan
              | GilgitBaltistan
              | Pakhtunkhwa
              | Punjab
              | Sindh
              deriving stock (Eq, Show, Ord, Generic, Bounded, Enum)

newtype District = District Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype UnionCouncil = UnionCouncil Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype CommonName = CommonName Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

data GPSCoordinates = GPSCoordinates R R
  deriving stock (Eq, Show, Generic)

newtype NumberOfPeople = NumberOfPeople Int
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord, Num)
  deriving anyclass (Newtype)

data Location = Location
  { province :: Province
  , district :: District
  , unionCouncil :: UnionCouncil
  , commonName :: CommonName
  , gpsCoordinates :: GPSCoordinates
  } deriving stock (Eq, Show, Generic)

data Affectees = Affectees
  { affecteesLocation :: Location
  , howMany :: NumberOfPeople
  , foodShortage :: NumberOfPeople
  , tentShortage :: NumberOfPeople
  , medicineShortage :: NumberOfPeople
  } deriving stock (Eq, Show, Generic)

newtype PersonName = PersonName Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype CNIC = CNIC Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype Phone = Phone Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype IBAN = IBAN Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype IBFT = IBFT Text
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

newtype Bank = Bank Text  
  deriving stock (Generic, Show)
  deriving newtype (Eq, Ord)
  deriving anyclass (Newtype)

data BankAccountAddress = Raast Phone
                        | IBFTAddress IBAN
                        | IBANAddress IBFT
                        | MobileAccount Phone
                        deriving stock (Eq, Show, Generic)

data ResponsibleParty = ResponsibleParty
  { aapkaNaam :: PersonName
  , located :: Location 
  , phone :: Phone
  , cnic :: CNIC
  , bank :: Bank
  , bankAccount :: BankAccountAddress
  } deriving stock (Eq, Show, Generic)

data Atiya = Paisay Cost | Item AidItem
  deriving stock (Eq, Show, Generic)
