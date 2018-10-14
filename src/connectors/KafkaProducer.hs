module KafkaProducer where

import Control.Exception     (bracket)
import Control.Monad         (forM_)
import Data.ByteString       (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Kafka.Producer
import Data.Text             (Text)
import ConfigurationUtils

startProducer :: IO ()
startProducer = do producerProps <- createProducerProperties
                   kafkaProducerEither <-  newProducer producerProps
                   either <- case kafkaProducerEither of
                                  Right kafkaProducer -> prepareMessage kafkaProducer
                                  Left err -> do return $ Left err
                   print either

{-| Monad to create the ProducerProperties used to create the producerRecord.
    It contains the bootstrap server host and port, the timeout of connection a callback after we push the message
    and the level of logs-}
createProducerProperties :: IO ProducerProperties
createProducerProperties = do bootstrapServer <- getBootstrapServer
                              return $ brokersList [BrokerAddress bootstrapServer]
                               <> sendTimeout (Timeout 10000)
                               <> setCallback (deliveryCallback print)
                               <> logLevel KafkaLogDebug

{-| Function that receive a KafkaProducer which we use to send the message and close the producer after that.-}
prepareMessage :: KafkaProducer -> IO (Either KafkaError ())
prepareMessage kafkaProducer =  do result <- sendMessages kafkaProducer
                                   _ <- closeProducer kafkaProducer
                                   return result

{-| Function that receive a Kafka producer and using [produceMessage] passing a ProducerRecord it send the message to Kafka-}
sendMessages :: KafkaProducer -> IO (Either KafkaError ())
sendMessages kafkaProducer = do producerRecord <- createProducerRecord Nothing (Just $ pack "ohhhhhhh again")
                                maybeKafkaError <- produceMessage kafkaProducer producerRecord
                                forM_ maybeKafkaError print
                                return $ Right ()

{-| Function with key and value Maybe to fill together with the topic name where to send the message-}
createProducerRecord :: Maybe ByteString -> Maybe ByteString -> IO ProducerRecord
createProducerRecord key value = do topic <- getTopic
                                    return ProducerRecord { prTopic = TopicName topic, prPartition = UnassignedPartition, prKey = key , prValue = value }

{-| ---------------------
       CONFIGURATION
    ---------------------}
{-| Configuration monads section where we extract kafka configuration from config file-}

kafkaConnectorCfg = "$(HOME)/Development/Dive_into_Haskell/kafkaConnector.cfg" :: String

getTopic :: IO String
getTopic = getConfigParam kafkaConnectorCfg "topic"

getBootstrapServer :: IO String
getBootstrapServer = getConfigParam kafkaConnectorCfg "bootstrapServer"