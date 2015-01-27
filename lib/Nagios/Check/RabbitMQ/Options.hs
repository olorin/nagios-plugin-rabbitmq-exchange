module Nagios.Check.RabbitMQ.Options where

import           Nagios.Check.RabbitMQ.Types
import           Options.Applicative

parseOptions :: IO CheckOptions
parseOptions = execParser optionParser

optionParser :: ParserInfo CheckOptions
optionParser =
    info (helper <*> checkOptions)
    (fullDesc <>
        progDesc "Nagios NRPE check for RabbitMQ queue length" <>
        header "nagios-check-rabbitmq-queue-length - checks queue length against threshold"
    )

checkOptions :: Parser CheckOptions
checkOptions = CheckOptions
    <$> strOption
        (long "hostname"
        <> short 'H'
        <> value "localhost"
        <> metavar "HOSTNAME")
    <*> strOption
        (long "exchange"
        <> short 'e'
        <> help "Name of the exchange to check")
    <*> (minThreshold <$> optional ( option auto
        ( long "minrate"
        <> short 'r'
        <> metavar "MINIMUM_RATE" )))
    <*> (maxThreshold <$> optional ( option auto
        ( long "maxrate"
        <> short 'R'
        <> metavar "MAXIMUM_RATE" )))
    <*> (minThreshold <$> optional ( option auto
        ( long "minincomingconn"
        <> short 'i'
        <> metavar "MINIMUM_INCOMING_CONNECTIONS" )))
    <*> (minThreshold <$> optional ( option auto
        ( long "minoutgoingconn"
        <> short 'o'
        <> metavar "MINIMUM_OUTGOING_CONNECTIONS" )))
