{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text             as T
import qualified Data.Vector           as V
import           Nagios.Check.RabbitMQ
import           Network.HTTP.Client
import           System.Environment
import           System.Exit
import           System.Nagios.Plugin

main :: IO ()
main = runNagiosPlugin $ do
    CheckOptions{..} <- liftIO $ parseOptions

    username <- liftIO $ maybe "" BSC.pack <$> lookupEnv "RABBIT_USER"
    password <- liftIO $ maybe "" BSC.pack <$> lookupEnv "RABBIT_PASS"

    manager <- liftIO $ newManager defaultManagerSettings

    let baseUrl = concat [ "http://", hostname, "/api" ]

    -- Get the length of the response at /api/connections (perfdata only)
    let connUrl = concat [ baseUrl, "/connections" ]
    connRequest <- applyBasicAuth username password <$> parseUrl connUrl

    resp' <- liftIO $ httpLbs connRequest manager

    let connCount = case eitherDecode (responseBody resp') of
            Left e          -> fromIntegral 0
	    Right (Array x) -> (fromIntegral (V.length x))

    -- Full Exchange rates check
    let rateUrl = concat [ baseUrl, "/exchanges/%2F/", exchange ]
    authedRequest <- applyBasicAuth username password <$> parseUrl rateUrl

    let q_params = [ ("lengths_age",    Just "60")
                   , ("msg_rates_age",  Just "60")
                   , ("msg_rates_incr", Just "60")
                   ]
    let q_authedRequest = setQueryString q_params authedRequest

    resp <- liftIO $ httpLbs q_authedRequest manager

    case eitherDecode (responseBody resp) of
        Left e -> addResult Unknown $ T.pack ( "Exchange decode failed with: " ++ e )
        Right MessageDetail{..} -> do
	    addResult OK "Exchange rate within bounds"
	    addPerfDatum "rateConfirms"    (RealValue     rateConfirms)   NullUnit Nothing Nothing Nothing Nothing
	    addPerfDatum "ratePublishIn"   (RealValue     ratePublishIn)  NullUnit Nothing Nothing Nothing Nothing
	    addPerfDatum "ratePublishOut"  (RealValue     ratePublishOut) NullUnit Nothing Nothing Nothing Nothing
	    addPerfDatum "connectionCount" (IntegralValue connCount) NullUnit Nothing Nothing Nothing Nothing

	    --- Check options, if available
	    unless (rateConfirms `inBoundsOf` minWarning &&
		    rateConfirms `inBoundsOf` maxWarning)
		   (addResult Warning "Confirm Rate out of bounds")

	    unless (rateConfirms `inBoundsOf` minCritical &&
		    rateConfirms `inBoundsOf` maxCritical)
		   (addResult Critical "Confirm Rate out of bounds")

