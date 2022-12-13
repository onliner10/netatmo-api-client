{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Netatmo.Model
import Netatmo.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy NADashboardData)
      propMimeEq MimeJSON (Proxy :: Proxy NADate)
      propMimeEq MimeJSON (Proxy :: Proxy NADevice)
      propMimeEq MimeJSON (Proxy :: Proxy NADeviceListBody)
      propMimeEq MimeJSON (Proxy :: Proxy NADeviceListResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAHealthyHomeCoach)
      propMimeEq MimeJSON (Proxy :: Proxy NAHealthyHomeCoachDataBody)
      propMimeEq MimeJSON (Proxy :: Proxy NAHealthyHomeCoachDataResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAHeatingSystem)
      propMimeEq MimeJSON (Proxy :: Proxy NAHouseModel)
      propMimeEq MimeJSON (Proxy :: Proxy NAHouseModelCa)
      propMimeEq MimeJSON (Proxy :: Proxy NALinkStation)
      propMimeEq MimeJSON (Proxy :: Proxy NAMain)
      propMimeEq MimeJSON (Proxy :: Proxy NAMeasure)
      propMimeEq MimeJSON (Proxy :: Proxy NAMeasureBodyElem)
      propMimeEq MimeJSON (Proxy :: Proxy NAMeasureResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAModule)
      propMimeEq MimeJSON (Proxy :: Proxy NANewScheduleBody)
      propMimeEq MimeJSON (Proxy :: Proxy NANewScheduleResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAObject)
      propMimeEq MimeJSON (Proxy :: Proxy NAOkResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAPartnerDevicesResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAPidAlgo)
      propMimeEq MimeJSON (Proxy :: Proxy NAPlace)
      propMimeEq MimeJSON (Proxy :: Proxy NAPlug)
      propMimeEq MimeJSON (Proxy :: Proxy NAPublicData)
      propMimeEq MimeJSON (Proxy :: Proxy NAPublicDataResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NASetpoint)
      propMimeEq MimeJSON (Proxy :: Proxy NASetpointLog)
      propMimeEq MimeJSON (Proxy :: Proxy NASimpleAlgo)
      propMimeEq MimeJSON (Proxy :: Proxy NAStation)
      propMimeEq MimeJSON (Proxy :: Proxy NAStationDataBody)
      propMimeEq MimeJSON (Proxy :: Proxy NAStationDataResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAStationModule)
      propMimeEq MimeJSON (Proxy :: Proxy NAThermMeasure)
      propMimeEq MimeJSON (Proxy :: Proxy NAThermProgram)
      propMimeEq MimeJSON (Proxy :: Proxy NAThermStateBody)
      propMimeEq MimeJSON (Proxy :: Proxy NAThermStateResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAThermostat)
      propMimeEq MimeJSON (Proxy :: Proxy NAThermostatDataBody)
      propMimeEq MimeJSON (Proxy :: Proxy NAThermostatDataResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NATimeTableItem)
      propMimeEq MimeJSON (Proxy :: Proxy NAUser)
      propMimeEq MimeJSON (Proxy :: Proxy NAUserAdministrative)
      propMimeEq MimeJSON (Proxy :: Proxy NAUserResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeCamera)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeEvent)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeEventData)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeEventResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeFace)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeGlobalInfo)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeHome)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeHomeData)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeHomeDataResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeModule)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomePerson)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomePersonsAwayResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomePersonsHomeResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomePlace)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeSnapshot)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeUser)
      propMimeEq MimeJSON (Proxy :: Proxy NAWelcomeWebhookResponse)
      propMimeEq MimeJSON (Proxy :: Proxy NAYearMonth)
      propMimeEq MimeJSON (Proxy :: Proxy NAZone)
      
