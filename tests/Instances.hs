{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Netatmo.Model
import Netatmo.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary NADashboardData where
  arbitrary = sized genNADashboardData

genNADashboardData :: Int -> Gen NADashboardData
genNADashboardData n =
  NADashboardData
    <$> arbitraryReducedMaybe n -- nADashboardDataTimeUtc :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataDeviceId :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataBoilerOn :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataBoilerOff :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataTempTrend :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADashboardDataDateMaxTemp :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataDateMinTemp :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataMinTemp :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataMaxTemp :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataAbsolutePressure :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataCo2 :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataHumidity :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataNoise :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataPressure :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataPressureTrend :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADashboardDataRain :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataSumRain1 :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataSumRain24 :: Maybe Float
    <*> arbitraryReducedMaybe n -- nADashboardDataWindAngle :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataGustAngle :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataWindStrength :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataMaxWindStr :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataDateMaxWindStr :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataGustStrength :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADashboardDataHealthIdx :: Maybe Int
  
instance Arbitrary NADate where
  arbitrary = sized genNADate

genNADate :: Int -> Gen NADate
genNADate n =
  NADate
    <$> arbitraryReducedMaybe n -- nADateSec :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADateUsec :: Maybe Int
  
instance Arbitrary NADevice where
  arbitrary = sized genNADevice

genNADevice :: Int -> Gen NADevice
genNADevice n =
  NADevice
    <$> arbitraryReducedMaybe n -- nADeviceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADeviceBehavior :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceCipherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADeviceDateSetup :: Maybe NADate
    <*> arbitraryReducedMaybe n -- nADeviceFirmware :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceHwVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceHeatingSystem :: Maybe NAHeatingSystem
    <*> arbitraryReducedMaybe n -- nADeviceHouseModel :: Maybe NAHouseModel
    <*> arbitraryReducedMaybe n -- nADeviceIp :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADeviceLastFwUpdate :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceLastRadioStore :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceLastStatusStore :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceLastUpgrade :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceModuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADeviceModules :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nADevicePlace :: Maybe NAPlace
    <*> arbitraryReducedMaybeValue n -- nADeviceSetpoint :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- nADeviceSetpointDefaultDuration :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceSetpointOrder :: Maybe (Map.Map String [NAObject])
    <*> arbitraryReducedMaybe n -- nADevicePublicExtData :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nADeviceStationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADeviceThermProgram :: Maybe (Map.Map String NAThermProgram)
    <*> arbitraryReducedMaybe n -- nADeviceThermProgramBackup :: Maybe (Map.Map String [NAThermProgram])
    <*> arbitraryReducedMaybe n -- nADeviceThermProgramOrder :: Maybe (Map.Map String [NAObject])
    <*> arbitraryReducedMaybe n -- nADeviceType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADeviceUserOwner :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nADeviceUdpConn :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nADeviceWifiStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nADeviceNewFeatureAvail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nADeviceFirstPidAvail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nADeviceDashboardData :: Maybe NADashboardData
    <*> arbitraryReducedMaybe n -- nADeviceDataType :: Maybe [Text]
  
instance Arbitrary NADeviceListBody where
  arbitrary = sized genNADeviceListBody

genNADeviceListBody :: Int -> Gen NADeviceListBody
genNADeviceListBody n =
  NADeviceListBody
    <$> arbitraryReducedMaybe n -- nADeviceListBodyDevices :: Maybe [NADevice]
    <*> arbitraryReducedMaybe n -- nADeviceListBodyModules :: Maybe [NAModule]
  
instance Arbitrary NADeviceListResponse where
  arbitrary = sized genNADeviceListResponse

genNADeviceListResponse :: Int -> Gen NADeviceListResponse
genNADeviceListResponse n =
  NADeviceListResponse
    <$> arbitraryReducedMaybe n -- nADeviceListResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nADeviceListResponseBody :: Maybe NADeviceListBody
    <*> arbitraryReducedMaybe n -- nADeviceListResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nADeviceListResponseTimeServer :: Maybe Int
  
instance Arbitrary NAHealthyHomeCoach where
  arbitrary = sized genNAHealthyHomeCoach

genNAHealthyHomeCoach :: Int -> Gen NAHealthyHomeCoach
genNAHealthyHomeCoach n =
  NAHealthyHomeCoach
    <$> arbitraryReducedMaybe n -- nAHealthyHomeCoachId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachCipherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachLastStatusStore :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachPlace :: Maybe NAPlace
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachDashboardData :: Maybe NADashboardData
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachDataType :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachCo2Calibrating :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachDateSetup :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachLastSetup :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachFirmware :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachLastUpgrade :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachWifiStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachName :: Maybe Text
  
instance Arbitrary NAHealthyHomeCoachDataBody where
  arbitrary = sized genNAHealthyHomeCoachDataBody

genNAHealthyHomeCoachDataBody :: Int -> Gen NAHealthyHomeCoachDataBody
genNAHealthyHomeCoachDataBody n =
  NAHealthyHomeCoachDataBody
    <$> arbitraryReducedMaybe n -- nAHealthyHomeCoachDataBodyDevices :: Maybe [NAHealthyHomeCoach]
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachDataBodyUser :: Maybe NAUser
  
instance Arbitrary NAHealthyHomeCoachDataResponse where
  arbitrary = sized genNAHealthyHomeCoachDataResponse

genNAHealthyHomeCoachDataResponse :: Int -> Gen NAHealthyHomeCoachDataResponse
genNAHealthyHomeCoachDataResponse n =
  NAHealthyHomeCoachDataResponse
    <$> arbitraryReducedMaybe n -- nAHealthyHomeCoachDataResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachDataResponseBody :: Maybe NAHealthyHomeCoachDataBody
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachDataResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAHealthyHomeCoachDataResponseTimeServer :: Maybe Int
  
instance Arbitrary NAHeatingSystem where
  arbitrary = sized genNAHeatingSystem

genNAHeatingSystem :: Int -> Gen NAHeatingSystem
genNAHeatingSystem n =
  NAHeatingSystem
    <$> arbitraryReducedMaybe n -- nAHeatingSystemHeatingEnergy :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHeatingSystemHeatingSystemWindowSeen :: Maybe Bool
  
instance Arbitrary NAHouseModel where
  arbitrary = sized genNAHouseModel

genNAHouseModel :: Int -> Gen NAHouseModel
genNAHouseModel n =
  NAHouseModel
    <$> arbitraryReducedMaybe n -- nAHouseModelAlgoType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHouseModelCa :: Maybe NAHouseModelCa
    <*> arbitraryReducedMaybe n -- nAHouseModelExtTemps :: Maybe (Map.Map String Int)
    <*> arbitraryReducedMaybe n -- nAHouseModelFirstAnticipateAvail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAHouseModelFirstPidAvail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAHouseModelGefs :: Maybe NAStation
    <*> arbitraryReducedMaybe n -- nAHouseModelInUse :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHouseModelLastComputeTry :: Maybe NADate
    <*> arbitraryReducedMaybe n -- nAHouseModelLinkStation :: Maybe NALinkStation
    <*> arbitraryReducedMaybe n -- nAHouseModelPidAlgo :: Maybe NAPidAlgo
    <*> arbitraryReducedMaybe n -- nAHouseModelStation :: Maybe NAStation
    <*> arbitraryReducedMaybe n -- nAHouseModelPreferedAlgoType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAHouseModelSimpleAlgo :: Maybe NASimpleAlgo
    <*> arbitraryReducedMaybe n -- nAHouseModelTime :: Maybe NADate
    <*> arbitraryReducedMaybe n -- nAHouseModelStationFirsttimeAnticipate :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAHouseModelTimeAlgoChanged :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHouseModelTimePidComputed :: Maybe Int
  
instance Arbitrary NAHouseModelCa where
  arbitrary = sized genNAHouseModelCa

genNAHouseModelCa :: Int -> Gen NAHouseModelCa
genNAHouseModelCa n =
  NAHouseModelCa
    <$> arbitraryReducedMaybe n -- nAHouseModelCaTe :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAHouseModelCaTi :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAHouseModelCaSo :: Maybe Text
  
instance Arbitrary NALinkStation where
  arbitrary = sized genNALinkStation

genNALinkStation :: Int -> Gen NALinkStation
genNALinkStation n =
  NALinkStation
    <$> arbitraryReducedMaybe n -- nALinkStationMac :: Maybe Text
    <*> arbitraryReducedMaybe n -- nALinkStationExt :: Maybe Text
    <*> arbitraryReducedMaybe n -- nALinkStationTemperature :: Maybe Float
  
instance Arbitrary NAMain where
  arbitrary = sized genNAMain

genNAMain :: Int -> Gen NAMain
genNAMain n =
  NAMain
    <$> arbitraryReducedMaybe n -- nAMainId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAMainCipherId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAMainLastStatusStore :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAMainModules :: Maybe [NAStationModule]
    <*> arbitraryReducedMaybe n -- nAMainPlace :: Maybe NAPlace
    <*> arbitraryReducedMaybe n -- nAMainStationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAMainType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAMainDashboardData :: Maybe NADashboardData
    <*> arbitraryReducedMaybe n -- nAMainDataType :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAMainCo2Calibrating :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAMainDateSetup :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAMainLastSetup :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAMainModuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAMainFirmware :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAMainLastUpgrade :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAMainWifiStatus :: Maybe Int
  
instance Arbitrary NAMeasure where
  arbitrary = sized genNAMeasure

genNAMeasure :: Int -> Gen NAMeasure
genNAMeasure n =
  NAMeasure
    <$> arbitraryReducedMaybeValue n -- nAMeasureRes :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- nAMeasureType :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAMeasureRain60min :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureRain24h :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureRainLive :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureRainUtc :: Maybe Integer
    <*> arbitraryReducedMaybe n -- nAMeasureWindStrength :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureWindAngle :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureGustStrength :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureGustAngle :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureWindTimeutc :: Maybe Integer
  
instance Arbitrary NAMeasureBodyElem where
  arbitrary = sized genNAMeasureBodyElem

genNAMeasureBodyElem :: Int -> Gen NAMeasureBodyElem
genNAMeasureBodyElem n =
  NAMeasureBodyElem
    <$> arbitraryReducedMaybe n -- nAMeasureBodyElemBegTime :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAMeasureBodyElemStepTime :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAMeasureBodyElemValue :: Maybe [[Float]]
  
instance Arbitrary NAMeasureResponse where
  arbitrary = sized genNAMeasureResponse

genNAMeasureResponse :: Int -> Gen NAMeasureResponse
genNAMeasureResponse n =
  NAMeasureResponse
    <$> arbitraryReducedMaybe n -- nAMeasureResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAMeasureResponseBody :: Maybe [NAMeasureBodyElem]
    <*> arbitraryReducedMaybe n -- nAMeasureResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAMeasureResponseTimeServer :: Maybe Int
  
instance Arbitrary NAModule where
  arbitrary = sized genNAModule

genNAModule :: Int -> Gen NAModule
genNAModule n =
  NAModule
    <$> arbitraryReducedMaybe n -- nAModuleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAModuleDateSetup :: Maybe NADate
    <*> arbitraryReducedMaybe n -- nAModuleFirmware :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModuleType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAModuleModuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAModuleMainDevice :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAModuleLastMessage :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModuleLastSeen :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModulePublicExtData :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAModuleRfStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModuleBatteryVp :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModuleBatteryPercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModuleThermOrientation :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModuleThermRelayCmd :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAModuleDashboardData :: Maybe NADashboardData
    <*> arbitraryReducedMaybe n -- nAModuleDataType :: Maybe [Text]
  
instance Arbitrary NANewScheduleBody where
  arbitrary = sized genNANewScheduleBody

genNANewScheduleBody :: Int -> Gen NANewScheduleBody
genNANewScheduleBody n =
  NANewScheduleBody
    <$> arbitraryReducedMaybe n -- nANewScheduleBodyScheduleId :: Maybe Text
  
instance Arbitrary NANewScheduleResponse where
  arbitrary = sized genNANewScheduleResponse

genNANewScheduleResponse :: Int -> Gen NANewScheduleResponse
genNANewScheduleResponse n =
  NANewScheduleResponse
    <$> arbitraryReducedMaybe n -- nANewScheduleResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nANewScheduleResponseBody :: Maybe NANewScheduleBody
    <*> arbitraryReducedMaybe n -- nANewScheduleResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nANewScheduleResponseTimeServer :: Maybe Int
  
instance Arbitrary NAObject where
  arbitrary = sized genNAObject

genNAObject :: Int -> Gen NAObject
genNAObject n =
  NAObject
    <$> arbitraryReducedMaybe n -- nAObjectId :: Maybe Text
  
instance Arbitrary NAOkResponse where
  arbitrary = sized genNAOkResponse

genNAOkResponse :: Int -> Gen NAOkResponse
genNAOkResponse n =
  NAOkResponse
    <$> arbitraryReducedMaybe n -- nAOkResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAOkResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAOkResponseTimeServer :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAOkResponseBody :: Maybe Text
  
instance Arbitrary NAPartnerDevicesResponse where
  arbitrary = sized genNAPartnerDevicesResponse

genNAPartnerDevicesResponse :: Int -> Gen NAPartnerDevicesResponse
genNAPartnerDevicesResponse n =
  NAPartnerDevicesResponse
    <$> arbitraryReducedMaybe n -- nAPartnerDevicesResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPartnerDevicesResponseBody :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAPartnerDevicesResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAPartnerDevicesResponseTimeServer :: Maybe Int
  
instance Arbitrary NAPidAlgo where
  arbitrary = sized genNAPidAlgo

genNAPidAlgo :: Int -> Gen NAPidAlgo
genNAPidAlgo n =
  NAPidAlgo
    <$> arbitraryReducedMaybe n -- nAPidAlgoGain :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPidAlgoPeriod :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPidAlgoTd :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPidAlgoTi :: Maybe Int
  
instance Arbitrary NAPlace where
  arbitrary = sized genNAPlace

genNAPlace :: Int -> Gen NAPlace
genNAPlace n =
  NAPlace
    <$> arbitraryReducedMaybe n -- nAPlaceCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPlaceAltitude :: Maybe Float
    <*> arbitraryReducedMaybe n -- nAPlaceCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPlaceFromIp :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAPlaceImproveLocProposed :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAPlaceLocation :: Maybe [Double]
    <*> arbitraryReducedMaybe n -- nAPlaceTimezone :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPlaceTrustLocation :: Maybe Bool
  
instance Arbitrary NAPlug where
  arbitrary = sized genNAPlug

genNAPlug :: Int -> Gen NAPlug
genNAPlug n =
  NAPlug
    <$> arbitraryReducedMaybe n -- nAPlugId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPlugFirmware :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPlugLastStatusStore :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPlugPlace :: Maybe NAPlace
    <*> arbitraryReducedMaybe n -- nAPlugStationName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPlugType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPlugWifiStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPlugPlugConnectedBoiler :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPlugUdpConn :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAPlugLastPlugSeen :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPlugLastBilan :: Maybe NAYearMonth
    <*> arbitraryReducedMaybe n -- nAPlugModules :: Maybe [NAThermostat]
    <*> arbitraryReducedMaybe n -- nAPlugSyncing :: Maybe Bool
  
instance Arbitrary NAPublicData where
  arbitrary = sized genNAPublicData

genNAPublicData :: Int -> Gen NAPublicData
genNAPublicData n =
  NAPublicData
    <$> arbitraryReducedMaybe n -- nAPublicDataId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPublicDataPlace :: Maybe NAPlace
    <*> arbitraryReducedMaybe n -- nAPublicDataMark :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAPublicDataModules :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAPublicDataModuleTypes :: Maybe (Map.Map String Text)
    <*> arbitraryReducedMaybe n -- nAPublicDataMeasures :: Maybe (Map.Map String NAMeasure)
  
instance Arbitrary NAPublicDataResponse where
  arbitrary = sized genNAPublicDataResponse

genNAPublicDataResponse :: Int -> Gen NAPublicDataResponse
genNAPublicDataResponse n =
  NAPublicDataResponse
    <$> arbitraryReducedMaybe n -- nAPublicDataResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAPublicDataResponseBody :: Maybe [NAPublicData]
    <*> arbitraryReducedMaybe n -- nAPublicDataResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAPublicDataResponseTimeServer :: Maybe Int
  
instance Arbitrary NASetpoint where
  arbitrary = sized genNASetpoint

genNASetpoint :: Int -> Gen NASetpoint
genNASetpoint n =
  NASetpoint
    <$> arbitraryReducedMaybe n -- nASetpointSetpointTemp :: Maybe Float
    <*> arbitraryReducedMaybe n -- nASetpointSetpointEndtime :: Maybe Int
    <*> arbitraryReducedMaybe n -- nASetpointSetpointMode :: Maybe Text
  
instance Arbitrary NASetpointLog where
  arbitrary = sized genNASetpointLog

genNASetpointLog :: Int -> Gen NASetpointLog
genNASetpointLog n =
  NASetpointLog
    <$> arbitraryReducedMaybe n -- nASetpointLogSetpoint :: Maybe NASetpoint
    <*> arbitraryReducedMaybe n -- nASetpointLogTimestamp :: Maybe Integer
  
instance Arbitrary NASimpleAlgo where
  arbitrary = sized genNASimpleAlgo

genNASimpleAlgo :: Int -> Gen NASimpleAlgo
genNASimpleAlgo n =
  NASimpleAlgo
    <$> arbitraryReducedMaybe n -- nASimpleAlgoHighDeadband :: Maybe Int
  
instance Arbitrary NAStation where
  arbitrary = sized genNAStation

genNAStation :: Int -> Gen NAStation
genNAStation n =
  NAStation
    <$> arbitraryReducedMaybe n -- nAStationLagDown :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationLagUp :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationOffOvershoot :: Maybe Float
    <*> arbitraryReducedMaybe n -- nAStationOnOvershoot :: Maybe Float
    <*> arbitraryReducedMaybe n -- nAStationPk :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationTau :: Maybe Int
  
instance Arbitrary NAStationDataBody where
  arbitrary = sized genNAStationDataBody

genNAStationDataBody :: Int -> Gen NAStationDataBody
genNAStationDataBody n =
  NAStationDataBody
    <$> arbitraryReducedMaybe n -- nAStationDataBodyDevices :: Maybe [NAMain]
    <*> arbitraryReducedMaybe n -- nAStationDataBodyUser :: Maybe NAUser
  
instance Arbitrary NAStationDataResponse where
  arbitrary = sized genNAStationDataResponse

genNAStationDataResponse :: Int -> Gen NAStationDataResponse
genNAStationDataResponse n =
  NAStationDataResponse
    <$> arbitraryReducedMaybe n -- nAStationDataResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAStationDataResponseBody :: Maybe NAStationDataBody
    <*> arbitraryReducedMaybe n -- nAStationDataResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAStationDataResponseTimeServer :: Maybe Int
  
instance Arbitrary NAStationModule where
  arbitrary = sized genNAStationModule

genNAStationModule :: Int -> Gen NAStationModule
genNAStationModule n =
  NAStationModule
    <$> arbitraryReducedMaybe n -- nAStationModuleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAStationModuleType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAStationModuleLastMessage :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationModuleLastSeen :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationModuleDashboardData :: Maybe NADashboardData
    <*> arbitraryReducedMaybe n -- nAStationModuleDataType :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAStationModuleModuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAStationModuleLastSetup :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationModuleBatteryVp :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationModuleBatteryPercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationModuleRfStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAStationModuleFirmware :: Maybe Int
  
instance Arbitrary NAThermMeasure where
  arbitrary = sized genNAThermMeasure

genNAThermMeasure :: Int -> Gen NAThermMeasure
genNAThermMeasure n =
  NAThermMeasure
    <$> arbitraryReducedMaybe n -- nAThermMeasureTime :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermMeasureTemperature :: Maybe Float
    <*> arbitraryReducedMaybe n -- nAThermMeasureSetpointTemp :: Maybe Float
  
instance Arbitrary NAThermProgram where
  arbitrary = sized genNAThermProgram

genNAThermProgram :: Int -> Gen NAThermProgram
genNAThermProgram n =
  NAThermProgram
    <$> arbitraryReducedMaybe n -- nAThermProgramProgramId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAThermProgramName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAThermProgramZones :: Maybe [NAZone]
    <*> arbitraryReducedMaybe n -- nAThermProgramTimetable :: Maybe [NATimeTableItem]
    <*> arbitraryReducedMaybe n -- nAThermProgramSelected :: Maybe Bool
  
instance Arbitrary NAThermStateBody where
  arbitrary = sized genNAThermStateBody

genNAThermStateBody :: Int -> Gen NAThermStateBody
genNAThermStateBody n =
  NAThermStateBody
    <$> arbitraryReducedMaybe n -- nAThermStateBodySetpoint :: Maybe NASetpoint
    <*> arbitraryReducedMaybe n -- nAThermStateBodySetpointOrder :: Maybe NASetpoint
    <*> arbitraryReducedMaybe n -- nAThermStateBodyThermProgram :: Maybe NAThermProgram
    <*> arbitraryReducedMaybe n -- nAThermStateBodyThermProgramBackup :: Maybe [NAThermProgram]
    <*> arbitraryReducedMaybe n -- nAThermStateBodyThermProgramOrder :: Maybe NAThermProgram
    <*> arbitraryReducedMaybe n -- nAThermStateBodyThermOrientation :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyThermRelayCmd :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyBatteryVp :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyBatteryPercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyRfStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyPlugConnectedBoiler :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyUdpConn :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAThermStateBodyLastThermSeen :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyLastPlugSeen :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyWifiStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermStateBodyMeasured :: Maybe NAThermMeasure
  
instance Arbitrary NAThermStateResponse where
  arbitrary = sized genNAThermStateResponse

genNAThermStateResponse :: Int -> Gen NAThermStateResponse
genNAThermStateResponse n =
  NAThermStateResponse
    <$> arbitraryReducedMaybe n -- nAThermStateResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAThermStateResponseBody :: Maybe NAThermStateBody
    <*> arbitraryReducedMaybe n -- nAThermStateResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAThermStateResponseTimeServer :: Maybe Int
  
instance Arbitrary NAThermostat where
  arbitrary = sized genNAThermostat

genNAThermostat :: Int -> Gen NAThermostat
genNAThermostat n =
  NAThermostat
    <$> arbitraryReducedMaybe n -- nAThermostatId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAThermostatFirmware :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAThermostatModuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAThermostatLastMessage :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatRfStatus :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatBatteryVp :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatBatteryPercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatSetpoint :: Maybe NASetpoint
    <*> arbitraryReducedMaybe n -- nAThermostatSetpointOrder :: Maybe NASetpoint
    <*> arbitraryReducedMaybe n -- nAThermostatThermOrientation :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatThermRelayCmd :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatLastThermSeen :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAThermostatMeasured :: Maybe NAThermMeasure
    <*> arbitraryReducedMaybe n -- nAThermostatSetpointHistory :: Maybe [NASetpointLog]
    <*> arbitraryReducedMaybe n -- nAThermostatThermProgramList :: Maybe [NAThermProgram]
  
instance Arbitrary NAThermostatDataBody where
  arbitrary = sized genNAThermostatDataBody

genNAThermostatDataBody :: Int -> Gen NAThermostatDataBody
genNAThermostatDataBody n =
  NAThermostatDataBody
    <$> arbitraryReducedMaybe n -- nAThermostatDataBodyDevices :: Maybe [NAPlug]
    <*> arbitraryReducedMaybe n -- nAThermostatDataBodyUser :: Maybe NAUser
  
instance Arbitrary NAThermostatDataResponse where
  arbitrary = sized genNAThermostatDataResponse

genNAThermostatDataResponse :: Int -> Gen NAThermostatDataResponse
genNAThermostatDataResponse n =
  NAThermostatDataResponse
    <$> arbitraryReducedMaybe n -- nAThermostatDataResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAThermostatDataResponseBody :: Maybe NAThermostatDataBody
    <*> arbitraryReducedMaybe n -- nAThermostatDataResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAThermostatDataResponseTimeServer :: Maybe Int
  
instance Arbitrary NATimeTableItem where
  arbitrary = sized genNATimeTableItem

genNATimeTableItem :: Int -> Gen NATimeTableItem
genNATimeTableItem n =
  NATimeTableItem
    <$> arbitraryReducedMaybe n -- nATimeTableItemId :: Maybe Int
    <*> arbitraryReducedMaybe n -- nATimeTableItemMOffset :: Maybe Int
  
instance Arbitrary NAUser where
  arbitrary = sized genNAUser

genNAUser :: Int -> Gen NAUser
genNAUser n =
  NAUser
    <$> arbitraryReducedMaybe n -- nAUserId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserAdministrative :: Maybe NAUserAdministrative
    <*> arbitraryReducedMaybe n -- nAUserDateCreation :: Maybe NADate
    <*> arbitraryReducedMaybe n -- nAUserDevices :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAUserFriendDevices :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- nAUserMail :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserTimelineNotRead :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAUserTimelineSize :: Maybe Int
  
instance Arbitrary NAUserAdministrative where
  arbitrary = sized genNAUserAdministrative

genNAUserAdministrative :: Int -> Gen NAUserAdministrative
genNAUserAdministrative n =
  NAUserAdministrative
    <$> arbitraryReducedMaybe n -- nAUserAdministrativeCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserAdministrativeLang :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserAdministrativeRegLocale :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserAdministrativeUnit :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserAdministrativeWindunit :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserAdministrativePressureunit :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserAdministrativeFeelLikeAlgo :: Maybe Text
  
instance Arbitrary NAUserResponse where
  arbitrary = sized genNAUserResponse

genNAUserResponse :: Int -> Gen NAUserResponse
genNAUserResponse n =
  NAUserResponse
    <$> arbitraryReducedMaybe n -- nAUserResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAUserResponseBody :: Maybe NAUser
    <*> arbitraryReducedMaybe n -- nAUserResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAUserResponseTimeServer :: Maybe Int
  
instance Arbitrary NAWelcomeCamera where
  arbitrary = sized genNAWelcomeCamera

genNAWelcomeCamera :: Int -> Gen NAWelcomeCamera
genNAWelcomeCamera n =
  NAWelcomeCamera
    <$> arbitraryReducedMaybe n -- nAWelcomeCameraId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeCameraType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeCameraStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeCameraVpnUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeCameraIsLocal :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAWelcomeCameraSdStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeCameraAlimStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeCameraName :: Maybe Text
  
instance Arbitrary NAWelcomeEvent where
  arbitrary = sized genNAWelcomeEvent

genNAWelcomeEvent :: Int -> Gen NAWelcomeEvent
genNAWelcomeEvent n =
  NAWelcomeEvent
    <$> arbitraryReducedMaybe n -- nAWelcomeEventId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventTime :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAWelcomeEventCameraId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventPersonId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventSnapshot :: Maybe NAWelcomeSnapshot
    <*> arbitraryReducedMaybe n -- nAWelcomeEventVideoId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventVideoStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventIsArrival :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAWelcomeEventMessage :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventSubType :: Maybe Int
  
instance Arbitrary NAWelcomeEventData where
  arbitrary = sized genNAWelcomeEventData

genNAWelcomeEventData :: Int -> Gen NAWelcomeEventData
genNAWelcomeEventData n =
  NAWelcomeEventData
    <$> arbitraryReducedMaybe n -- nAWelcomeEventDataEventsList :: Maybe [NAWelcomeEvent]
  
instance Arbitrary NAWelcomeEventResponse where
  arbitrary = sized genNAWelcomeEventResponse

genNAWelcomeEventResponse :: Int -> Gen NAWelcomeEventResponse
genNAWelcomeEventResponse n =
  NAWelcomeEventResponse
    <$> arbitraryReducedMaybe n -- nAWelcomeEventResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeEventResponseBody :: Maybe NAWelcomeEventData
    <*> arbitraryReducedMaybe n -- nAWelcomeEventResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAWelcomeEventResponseTimeServer :: Maybe Int
  
instance Arbitrary NAWelcomeFace where
  arbitrary = sized genNAWelcomeFace

genNAWelcomeFace :: Int -> Gen NAWelcomeFace
genNAWelcomeFace n =
  NAWelcomeFace
    <$> arbitraryReducedMaybe n -- nAWelcomeFaceId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeFaceVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAWelcomeFaceKey :: Maybe Text
  
instance Arbitrary NAWelcomeGlobalInfo where
  arbitrary = sized genNAWelcomeGlobalInfo

genNAWelcomeGlobalInfo :: Int -> Gen NAWelcomeGlobalInfo
genNAWelcomeGlobalInfo n =
  NAWelcomeGlobalInfo
    <$> arbitraryReducedMaybe n -- nAWelcomeGlobalInfoShowTags :: Maybe Bool
  
instance Arbitrary NAWelcomeHome where
  arbitrary = sized genNAWelcomeHome

genNAWelcomeHome :: Int -> Gen NAWelcomeHome
genNAWelcomeHome n =
  NAWelcomeHome
    <$> arbitraryReducedMaybe n -- nAWelcomeHomeId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeHomePersons :: Maybe [NAWelcomePerson]
    <*> arbitraryReducedMaybe n -- nAWelcomeHomePlace :: Maybe NAWelcomePlace
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeCameras :: Maybe [NAWelcomeCamera]
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeEvents :: Maybe [NAWelcomeEvent]
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeModules :: Maybe [NAWelcomeModule]
  
instance Arbitrary NAWelcomeHomeData where
  arbitrary = sized genNAWelcomeHomeData

genNAWelcomeHomeData :: Int -> Gen NAWelcomeHomeData
genNAWelcomeHomeData n =
  NAWelcomeHomeData
    <$> arbitraryReducedMaybe n -- nAWelcomeHomeDataHomes :: Maybe [NAWelcomeHome]
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeDataUser :: Maybe NAWelcomeUser
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeDataGlobalInfo :: Maybe NAWelcomeGlobalInfo
  
instance Arbitrary NAWelcomeHomeDataResponse where
  arbitrary = sized genNAWelcomeHomeDataResponse

genNAWelcomeHomeDataResponse :: Int -> Gen NAWelcomeHomeDataResponse
genNAWelcomeHomeDataResponse n =
  NAWelcomeHomeDataResponse
    <$> arbitraryReducedMaybe n -- nAWelcomeHomeDataResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeDataResponseBody :: Maybe NAWelcomeHomeData
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeDataResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAWelcomeHomeDataResponseTimeServer :: Maybe Int
  
instance Arbitrary NAWelcomeModule where
  arbitrary = sized genNAWelcomeModule

genNAWelcomeModule :: Int -> Gen NAWelcomeModule
genNAWelcomeModule n =
  NAWelcomeModule
    <$> arbitraryReducedMaybe n -- nAWelcomeModuleId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeModuleType :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeModuleName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeModuleBatteryPercent :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAWelcomeModuleStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeModuleRf :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeModuleLastActivity :: Maybe Int
  
instance Arbitrary NAWelcomePerson where
  arbitrary = sized genNAWelcomePerson

genNAWelcomePerson :: Int -> Gen NAWelcomePerson
genNAWelcomePerson n =
  NAWelcomePerson
    <$> arbitraryReducedMaybe n -- nAWelcomePersonId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomePersonLastSeen :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAWelcomePersonOutOfSight :: Maybe Bool
    <*> arbitraryReducedMaybe n -- nAWelcomePersonFace :: Maybe NAWelcomeFace
    <*> arbitraryReducedMaybe n -- nAWelcomePersonPseudo :: Maybe Text
  
instance Arbitrary NAWelcomePersonsAwayResponse where
  arbitrary = sized genNAWelcomePersonsAwayResponse

genNAWelcomePersonsAwayResponse :: Int -> Gen NAWelcomePersonsAwayResponse
genNAWelcomePersonsAwayResponse n =
  NAWelcomePersonsAwayResponse
    <$> arbitraryReducedMaybe n -- nAWelcomePersonsAwayResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomePersonsAwayResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAWelcomePersonsAwayResponseTimeServer :: Maybe Int
  
instance Arbitrary NAWelcomePersonsHomeResponse where
  arbitrary = sized genNAWelcomePersonsHomeResponse

genNAWelcomePersonsHomeResponse :: Int -> Gen NAWelcomePersonsHomeResponse
genNAWelcomePersonsHomeResponse n =
  NAWelcomePersonsHomeResponse
    <$> arbitraryReducedMaybe n -- nAWelcomePersonsHomeResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomePersonsHomeResponseTimeExec :: Maybe Double
    <*> arbitraryReducedMaybe n -- nAWelcomePersonsHomeResponseTimeServer :: Maybe Int
  
instance Arbitrary NAWelcomePlace where
  arbitrary = sized genNAWelcomePlace

genNAWelcomePlace :: Int -> Gen NAWelcomePlace
genNAWelcomePlace n =
  NAWelcomePlace
    <$> arbitraryReducedMaybe n -- nAWelcomePlaceCity :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomePlaceCountry :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomePlaceTimezone :: Maybe Text
  
instance Arbitrary NAWelcomeSnapshot where
  arbitrary = sized genNAWelcomeSnapshot

genNAWelcomeSnapshot :: Int -> Gen NAWelcomeSnapshot
genNAWelcomeSnapshot n =
  NAWelcomeSnapshot
    <$> arbitraryReducedMaybe n -- nAWelcomeSnapshotId :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeSnapshotVersion :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAWelcomeSnapshotKey :: Maybe Text
  
instance Arbitrary NAWelcomeUser where
  arbitrary = sized genNAWelcomeUser

genNAWelcomeUser :: Int -> Gen NAWelcomeUser
genNAWelcomeUser n =
  NAWelcomeUser
    <$> arbitraryReducedMaybe n -- nAWelcomeUserLang :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeUserRegLocale :: Maybe Text
  
instance Arbitrary NAWelcomeWebhookResponse where
  arbitrary = sized genNAWelcomeWebhookResponse

genNAWelcomeWebhookResponse :: Int -> Gen NAWelcomeWebhookResponse
genNAWelcomeWebhookResponse n =
  NAWelcomeWebhookResponse
    <$> arbitraryReducedMaybe n -- nAWelcomeWebhookResponseStatus :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAWelcomeWebhookResponseTimeExec :: Maybe Double
  
instance Arbitrary NAYearMonth where
  arbitrary = sized genNAYearMonth

genNAYearMonth :: Int -> Gen NAYearMonth
genNAYearMonth n =
  NAYearMonth
    <$> arbitraryReducedMaybe n -- nAYearMonthY :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAYearMonthM :: Maybe Int
  
instance Arbitrary NAZone where
  arbitrary = sized genNAZone

genNAZone :: Int -> Gen NAZone
genNAZone n =
  NAZone
    <$> arbitraryReducedMaybe n -- nAZoneId :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAZoneType :: Maybe Int
    <*> arbitraryReducedMaybe n -- nAZoneName :: Maybe Text
    <*> arbitraryReducedMaybe n -- nAZoneTemp :: Maybe Float
  



instance Arbitrary E'AppType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'RequiredData where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Scale where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SetpointMode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Type where
  arbitrary = arbitraryBoundedEnum

