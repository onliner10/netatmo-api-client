{-
   Netatmo

   <h3>Welcome to the Netatmo swagger on-line documentation !</h3> This site is a complement to the official <a href=\"https://dev.netatmo.com/\">Netatmo developper documentation</a> using swagger to bring interactivity and easy testing of requests with the \"try it\" button (authenticate with the authorization code OAuth2 flow by clicking the authenticate button in the methods). You can find the source code for this site can be found in the project <a href=\"https://github.com/cbornet/netatmo-swagger-ui\">netatmo-swagger-ui</a>. You can also use the online <a href=\"./swagger.json\">swagger declaration</a> file to generate code or static documentation (see <a href=\"https://github.com/cbornet/netatmo-swagger-api\">netatmo-swagger-api</a>). 

   OpenAPI Version: 3.0.1
   Netatmo API version: 1.1.3
   Contact: contact-api@netatmo.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Netatmo.API.Public
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Netatmo.API.Public where

import Netatmo.Core
import Netatmo.MimeTypes
import Netatmo.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** Public

-- *** getmeasure2

-- | @GET \/getmeasure@
-- 
-- The method getmeasure returns the measurements of a device or a module. 
-- 
-- AuthMethod: 'AuthOAuthCodeOauth', 'AuthOAuthPasswordOauth'
-- 
getmeasure2
  :: DeviceId -- ^ "deviceId" -  Id of the device whose module's measurements you want to retrieve. This _id can be found in the user's devices field.
  -> Scale -- ^ "scale" -  Defines the time interval between two measurements. Possible values : max -> every value stored will be returned 30min -> 1 value every 30 minutes 1hour -> 1 value every hour 3hours -> 1 value every 3 hours 1day -> 1 value per day 1week -> 1 value per week 1month -> 1 value per month 
  -> ParamType -- ^ "_type" -  Measures you are interested in. Data you can request depends on the scale. **For Weather Station:**   * max -> Temperature (??C), CO2 (ppm), Humidity (%), Pressure (mbar), Noise (db), Rain (mm), WindStrength (km/h), WindAngle (angles), Guststrength (km/h), GustAngle (angles)   * 30min, 1hour, 3hours -> Same as above + min_temp, max_temp, min_hum, max_hum, min_pressure, max_pressure, min_noise, max_noise, sum_rain, date_max_gust   * 1day, 1week, 1month -> Same as above + date_min_temp, date_max_temp, date_min_hum, date_max_hum, date_min_pressure, date_max_pressure, date_min_noise, date_max_noise, date_min_co2, date_max_co2  **For Thermostat:**   * max -> temperature (??C), sp_temperature (??C), boileron (sec), boileroff (sec)   * 30min, 1hour, 3hours -> temperature, sp_temperature, min_temp, max_temp, sum_boiler_on, sum_boiler_off   * 1day, 1week, 1month -> temperature, min_temp, date_min_temp, max_temp, sum_boiler_on, sum_boiler_off 
  -> NetatmoRequest Getmeasure2 MimeNoContent NAMeasureResponse MimeJSON
getmeasure2 (DeviceId deviceId) (Scale scale) (ParamType _type) =
  _mkRequest "GET" ["/getmeasure"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthCodeOauth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPasswordOauth)
    `addQuery` toQuery ("device_id", Just deviceId)
    `addQuery` toQuery ("scale", Just scale)
    `addQuery` toQueryColl CommaSeparated ("type", Just _type)

data Getmeasure2  

-- | /Optional Param/ "module_id" - If you don't specify any module_id you will retrieve the device's measurements. If you specify a module_id you will retrieve the module's measurements.
instance HasOptionalParam Getmeasure2 ModuleId where
  applyOptionalParam req (ModuleId xs) =
    req `addQuery` toQuery ("module_id", Just xs)

-- | /Optional Param/ "date_begin" - Starting timestamp (utc) of the requested measurements. Please note measurement retrieving is limited to 1024 measurements. 
instance HasOptionalParam Getmeasure2 DateBegin where
  applyOptionalParam req (DateBegin xs) =
    req `addQuery` toQuery ("date_begin", Just xs)

-- | /Optional Param/ "date_end" - Ending timestamp (utc) of the request measurements. If you want only the last measurement, do not provide date_begin, and set date_end to `last`. 
instance HasOptionalParam Getmeasure2 DateEnd where
  applyOptionalParam req (DateEnd xs) =
    req `addQuery` toQuery ("date_end", Just xs)

-- | /Optional Param/ "limit" - Limits the number of measurements returned (default & max is 1024)
instance HasOptionalParam Getmeasure2 Limit where
  applyOptionalParam req (Limit xs) =
    req `addQuery` toQuery ("limit", Just xs)

-- | /Optional Param/ "optimize" - Allows you to choose the format of the answer. If you build a mobile app and bandwith usage is an issue, use `optimize = true`. Use `optimize = false`, for an easier parse. In this case, values are indexed by sorted timestamp. Example of un-optimized response : ```json {\"status\": \"ok\",    \"body\": {     \"1347575400\": [18.3,39],     \"1347586200\": [20.6,48]   }, \"time_exec\": 0.012136936187744} ``` If optimize is set true, measurements are returned as an array of series of regularly spaced measurements. Each series is defined by a beginning time beg_time and a step between measurements, step_time: ```json {\"status\": \"ok\",   \"body\": [     {\"beg_time\": 1347575400,      \"step_time\": 10800,      \"value\":          [[18.3,39],         [ 20.6,48]]     }], \"time_exec\": 0.014238119125366} ``` Default value is `true`. 
instance HasOptionalParam Getmeasure2 Optimize where
  applyOptionalParam req (Optimize xs) =
    req `addQuery` toQuery ("optimize", Just xs)

-- | /Optional Param/ "real_time" - In scales higher than max, since the data is aggregated, the timestamps returned are by default offset by +(scale/2). For instance, if you ask for measurements at a daily scale, you will receive data timestamped at 12:00 if real_time is set to `false` (default case), and timestamped at 00:00 if real_time is set to `true`. NB : The servers always store data with real_time set to `true` and data are offset by this parameter AFTER having being time-filtered, thus you could have data after date_end if real_time is set to `false`. 
instance HasOptionalParam Getmeasure2 RealTime where
  applyOptionalParam req (RealTime xs) =
    req `addQuery` toQuery ("real_time", Just xs)
-- | @application/json@
instance Produces Getmeasure2 MimeJSON


-- *** getpublicdata

-- | @GET \/getpublicdata@
-- 
-- Retrieves publicly shared weather data from Outdoor Modules within a predefined area.
-- 
-- AuthMethod: 'AuthOAuthCodeOauth', 'AuthOAuthPasswordOauth'
-- 
getpublicdata
  :: LatNe -- ^ "latNe" -  Latitude of the north east corner of the requested area. -85 <= lat_ne <= 85 and lat_ne>lat_sw
  -> LonNe -- ^ "lonNe" -  Longitude of the north east corner of the requested area. -180 <= lon_ne <= 180 and lon_ne>lon_sw
  -> LatSw -- ^ "latSw" -  Latitude of the south west corner of the requested area. -85 <= lat_sw <= 85
  -> LonSw -- ^ "lonSw" -  Longitude of the south west corner of the requested area. -180 <= lon_sw <= 180
  -> NetatmoRequest Getpublicdata MimeNoContent NAPublicDataResponse MimeJSON
getpublicdata (LatNe latNe) (LonNe lonNe) (LatSw latSw) (LonSw lonSw) =
  _mkRequest "GET" ["/getpublicdata"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthCodeOauth)
    `_hasAuthType` (P.Proxy :: P.Proxy AuthOAuthPasswordOauth)
    `addQuery` toQuery ("lat_ne", Just latNe)
    `addQuery` toQuery ("lon_ne", Just lonNe)
    `addQuery` toQuery ("lat_sw", Just latSw)
    `addQuery` toQuery ("lon_sw", Just lonSw)

data Getpublicdata  

-- | /Optional Param/ "required_data" - To filter stations based on relevant measurements you want (e.g. rain will only return stations with rain gauges). Default is no filter. You can find all measurements available on the Thermostat page.
instance HasOptionalParam Getpublicdata RequiredData where
  applyOptionalParam req (RequiredData xs) =
    req `addQuery` toQueryColl CommaSeparated ("required_data", Just xs)

-- | /Optional Param/ "filter" - True to exclude stations with abnormal temperature measures. Default is false.
instance HasOptionalParam Getpublicdata Filter where
  applyOptionalParam req (Filter xs) =
    req `addQuery` toQuery ("filter", Just xs)
-- | @application/json@
instance Produces Getpublicdata MimeJSON

