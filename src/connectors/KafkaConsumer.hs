module KafkaConsumer where

{-| Author: Pablo Perez Garcia
    This code example is build in top of hw-kafka-client library https://hackage.haskell.org/package/hw-kafka-client
    Before start codgin remember that you need install in your system [librdkafka]
    git clone https://github.com/edenhill/librdkafka
    cd librdkafka
    ./configure
    make
    sudo make install
-}
import Control.Arrow     ((&&&))
import Control.Exception (bracket)
import Data.Monoid       ((<>))
import Kafka.Consumer
import qualified Data.Text as TEXT
import Data.ByteString
import qualified Data.ByteString.Char8 as CH
import qualified System.IO as SIO
import ConfigurationUtils
import Text.Read (lift)
import Control.Concurrent (threadDelay)

{-| IO monad to run the consumer, using the function [newConsumer] where we pass the ConsumerProperties and the Subscription
    it return a Either of [KafkaError] or [KafkaConsumer] -}
startConsumer :: IO ()
startConsumer = do consumerSubscription <- createConsumerSubscription
                   consumerProps <- createConsumerProp
                   kafkaConsumerEither <- newConsumer consumerProps consumerSubscription
                   response <- case kafkaConsumerEither of
                              Left err -> do return $ Left err
                              Right kafkaConsumer -> consumeMessages kafkaConsumer
                   print response

{-| We use [ConsumerProperties] adding information of the broker host, consumer group id, commit strategy and log level-}
createConsumerProp :: IO ConsumerProperties
createConsumerProp = do  groupIdName <- getGroupId
                         bootstrapServer <- getBootstrapServer
                         return $ brokersList [BrokerAddress bootstrapServer]
                                <> groupId (ConsumerGroupId groupIdName)
                                <> noAutoCommit
                                <> logLevel KafkaLogInfo

{-| Create a subscription using [createTopic] functions and is merged using [<>] operator with another subscription created
    when we run [OffsetReset] function to define the Offset strategy -}
createConsumerSubscription :: IO Subscription
createConsumerSubscription = do topic <- getTopic
                                return $ createTopic topic <> offsetReset Earliest

{-| Create a subscription using [topics] functions which expect a [TopicName] type created passing a topic namex -}
createTopic:: String  -> Subscription
createTopic topic = topics [TopicName topic]

{-| Having a Kafka consumer We use [mapM_] function to start iterating 15 times, polling for messages.
    We then commit offset using [commitAllOffsets] which return a maybe of Kafka error which in case is filled
        contains the error description-}
consumeMessages :: KafkaConsumer -> IO (Either KafkaError String)
consumeMessages kafkaConsumer = do message <- consumeMessage kafkaConsumer
                                   SIO.putStrLn $ "Message: " <> show message
                                   maybeError <- commitAllOffsets OffsetCommit kafkaConsumer
                                   SIO.putStrLn $ "Offsets: " <> maybe "Committed." show maybeError
                                   SIO.putStrLn $ "---------------------------------------"
                                   _ <- consumeMessages kafkaConsumer
                                   _ <- closeConsumer kafkaConsumer
                                   return $ Right "All events processed"

{-| Having a Kafka consumer We use [pollMessage] function passing the consumer and a timeout.
    It return an either of KafkaError or ConsumerRecord..-}
consumeMessage :: KafkaConsumer -> IO (Either KafkaError String)
consumeMessage kafkaConsumer = do eitherConsumerRecord <- pollMessage kafkaConsumer (Timeout 5000)
                                  eitherResponse <- case eitherConsumerRecord of
                                              Right consumerRecord -> getConsumerRecordValue consumerRecord
                                  return eitherResponse

{-| Function to extract the value from the ConsumerRecord -}
getConsumerRecordValue :: ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> IO (Either KafkaError String)
getConsumerRecordValue(ConsumerRecord _ _ _ _ _ value) = case value of
                                                  Just value -> return $ Right $ CH.unpack value
                                                  Nothing -> return $ Left $ KafkaError "No data find in Customer record"

