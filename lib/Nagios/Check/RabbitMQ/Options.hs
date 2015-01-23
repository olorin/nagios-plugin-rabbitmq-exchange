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
        ( long "minwarning"
        <> short 'w'
        <> metavar "MINIMUM_WARN" )))
    <*> (minThreshold <$> optional ( option auto
        ( long "mincritical"
        <> short 'c'
        <> metavar "MINIMUM_CRIT" )))
    <*> (maxThreshold <$> optional ( option auto
        ( long "maxwarning"
        <> short 'W'
        <> metavar "MAXIMUM_WARN" )))
    <*> (maxThreshold <$> optional ( option auto
        ( long "maxcritical"
        <> short 'C'
        <> metavar "MAXIMUM_CRIT" )))
