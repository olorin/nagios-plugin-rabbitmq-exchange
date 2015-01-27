{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans
import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text             as T
import           Nagios.Check.RabbitMQ
import           Network.HTTP.Client
import           System.Environment
import           System.Nagios.Plugin

simplePerfDatum :: T.Text -> PerfValue -> NagiosPlugin()
simplePerfDatum n p = addPerfDatum n p NullUnit Nothing Nothing Nothing Nothing

main :: IO ()
main = runNagiosPlugin $ do
    CheckOptions{..} <- liftIO parseOptions

    username <- liftIO $ maybe "" BSC.pack <$> lookupEnv "RABBIT_USER"
    password <- liftIO $ maybe "" BSC.pack <$> lookupEnv "RABBIT_PASS"

    manager <- liftIO $ newManager defaultManagerSettings

    let rateUrl = concat [ "http://", hostname, "/api/exchanges/%2F/", exchange ]
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

	    simplePerfDatum "rateConfirms"        $ RealValue rateConfirms
	    simplePerfDatum "ratePublishIn"       $ RealValue ratePublishIn
	    simplePerfDatum "ratePublishOut"      $ RealValue ratePublishOut

            let countIncoming = length connectionsIncoming
            let countOutgoing = length connectionsOutgoing

	    simplePerfDatum "connectionsIncoming" $ IntegralValue . fromIntegral $ countIncoming
	    simplePerfDatum "connectionsOutgoing" $ IntegralValue . fromIntegral $ countOutgoing

	    --- Check options, if available
	    unless (rateConfirms `inBoundsOf` minRate && rateConfirms `inBoundsOf` maxRate)
		   (addResult Critical "Confirm Rate out of bounds")

	    unless (fromIntegral countIncoming `inBoundsOf` minIncomingConn)
	           (addResult Critical "Incoming connection rate out of bounds")

	    unless (fromIntegral countOutgoing `inBoundsOf` minOutgoingConn)
	           (addResult Critical "Outgoing connection rate out of bounds")

