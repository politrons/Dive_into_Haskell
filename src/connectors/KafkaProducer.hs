module KafkaProducer where

{-| Author: Pablo Perez Garcia
    This code example is build in top of hw-kafka-client library https://hackage.haskell.org/package/hw-kafka-client
    Before start codgin remember that you need install in your system [librdkafka]
    git clone https://github.com/edenhill/librdkafka
    cd librdkafka
    ./configure
    make
    sudo make install
-}
import Control.Exception     (bracket)
import Control.Monad         (forM_)
import Data.ByteString       (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Kafka.Producer
import Data.Text             (Text)
import ConfigurationUtils

{-| Monad to send a message to Kafka. We use the function [newProducer] passing the ProducerProperties
    to obtain an Either of KafkaError or KafkaProducer -}
startProducer :: String -> IO ()
startProducer message = do producerProps <- createProducerProperties
                           kafkaProducerEither <- newProducer producerProps
                           either <- case kafkaProducerEither of
                                          Right kafkaProducer -> prepareMessage kafkaProducer message
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
prepareMessage ::   KafkaProducer -> String -> IO (Either KafkaError ())
prepareMessage kafkaProducer message=  do result <- sendMessages kafkaProducer message
                                          _ <- closeProducer kafkaProducer
                                          return result

{-| Function that receive a Kafka producer and using [produceMessage] passing a ProducerRecord it send the message to Kafka-}
sendMessages ::  KafkaProducer -> String -> IO (Either KafkaError ())
sendMessages kafkaProducer message= do producerRecord <- createProducerRecord Nothing (Just $ pack message)
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