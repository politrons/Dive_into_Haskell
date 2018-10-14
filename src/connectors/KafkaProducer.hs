module KafkaProducer where

import Control.Exception     (bracket)
import Control.Monad         (forM_)
import Data.ByteString       (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Monoid
import Kafka.Producer
import Data.Text             (Text)

startProducer :: IO ()
startProducer = do kafkaProducerEither <-  newProducer producerProps
                   either <- case kafkaProducerEither of
                                  Right kafkaProducer -> do result <- sendMessages kafkaProducer
                                                            _ <- closeProducer kafkaProducer
                                                            return result
                                  Left err -> do return $ Left err
                   print either

producerProps :: ProducerProperties
producerProps = brokersList [BrokerAddress "localhost:9092"]
             <> sendTimeout (Timeout 10000)
             <> setCallback (deliveryCallback print)
             <> logLevel KafkaLogDebug

sendMessages :: KafkaProducer -> IO (Either KafkaError ())
sendMessages prod = do maybeKafkaError <- produceMessage prod (createProducerRecord Nothing (Just $ pack "Super cool producer"))
                       forM_ maybeKafkaError print
                       return $ Right ()

{-| Function with key and value Maybe to fill together with the topic name where to send the message-}
createProducerRecord :: Maybe ByteString -> Maybe ByteString -> ProducerRecord
createProducerRecord k v = ProducerRecord
                  { prTopic = targetTopic
                  , prPartition = UnassignedPartition
                  , prKey = k
                  , prValue = v
                  }

targetTopic :: TopicName
targetTopic = TopicName "MyFirstTopic"