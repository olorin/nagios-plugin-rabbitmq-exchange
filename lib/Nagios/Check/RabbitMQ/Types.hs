{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Nagios.Check.RabbitMQ.Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import           Data.Int
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics

data Threshold = NoThreshold
               | MinThreshold Double
               | MaxThreshold Double
  deriving Show

inBoundsOf :: Double -> Threshold -> Bool
_ `inBoundsOf` NoThreshold      = True
x `inBoundsOf` (MinThreshold y) = x >= y
x `inBoundsOf` (MaxThreshold y) = x <= y

minThreshold :: Maybe Double -> Threshold
minThreshold Nothing  = NoThreshold
minThreshold (Just x) = MinThreshold x

maxThreshold :: Maybe Double -> Threshold
maxThreshold Nothing  = NoThreshold
maxThreshold (Just x) = MinThreshold x

data CheckOptions = CheckOptions
    { hostname    :: String
    , exchange    :: String
    , minWarning  :: Threshold
    , minCritical :: Threshold
    , maxWarning  :: Threshold
    , maxCritical :: Threshold
    } deriving Show

data ConnectionDetail = ConnectionDetail
    { publish :: Double } deriving (Show, Generic)

instance FromJSON ConnectionDetail where
    parseJSON (Object o) = ConnectionDetail
         <$> ((o .: "stats") >>= (.: "publish"))

data MessageDetail = MessageDetail
    { rateConfirms        :: Double
    , ratePublishIn       :: Double
    , ratePublishOut      :: Double
    , connectionsIncoming :: [ConnectionDetail]
    , connectionsOutgoing :: [ConnectionDetail]
    } deriving (Show,Generic)


instance FromJSON MessageDetail where
    parseJSON (Object o) = MessageDetail
	<$> ((o .: "message_stats") >>= (.: "confirm_details") >>= (.: "avg_rate"))
	<*> ((o .: "message_stats") >>= (.: "publish_in_details") >>= (.: "avg_rate"))
	<*> ((o .: "message_stats") >>= (.: "publish_out_details") >>= (.: "avg_rate"))
	<*> ((o .: "incoming"))
	<*> ((o .: "outgoing"))
