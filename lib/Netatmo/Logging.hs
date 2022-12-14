{-
   Netatmo

   <h3>Welcome to the Netatmo swagger on-line documentation !</h3> This site is a complement to the official <a href=\"https://dev.netatmo.com/\">Netatmo developper documentation</a> using swagger to bring interactivity and easy testing of requests with the \"try it\" button (authenticate with the authorization code OAuth2 flow by clicking the authenticate button in the methods). You can find the source code for this site can be found in the project <a href=\"https://github.com/cbornet/netatmo-swagger-ui\">netatmo-swagger-ui</a>. You can also use the online <a href=\"./swagger.json\">swagger declaration</a> file to generate code or static documentation (see <a href=\"https://github.com/cbornet/netatmo-swagger-api\">netatmo-swagger-api</a>). 

   OpenAPI Version: 3.0.1
   Netatmo API version: 1.1.3
   Contact: contact-api@netatmo.com
   Generated by OpenAPI Generator (https://openapi-generator.tech)
-}

{-|
Module : Netatmo.Logging
Logging functions
-}
{-# LANGUAGE CPP #-}

#ifdef USE_KATIP

module Netatmo.Logging
  ( module Netatmo.LoggingKatip
  ) where

import Netatmo.LoggingKatip

#else

module Netatmo.Logging
  ( module Netatmo.LoggingMonadLogger
  ) where

import Netatmo.LoggingMonadLogger

#endif
