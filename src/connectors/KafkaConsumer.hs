module KafkaConsumer where

{-| This code example is build in top of hw-kafka-client library https://hackage.haskell.org/package/hw-kafka-client
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
import Data.Text         (Text)
import Data.ByteString
import qualified Data.ByteString.Char8 as CH
import qualified System.IO as SIO

startConsumer :: IO ()
startConsumer = do kafkaConsumerEither <- newConsumer consumerProps createConsumerSubscription
                   response <- case kafkaConsumerEither of
                              Left err -> do return $ Left err
                              Right kafkaConsumer -> consumeMessages kafkaConsumer
                   print response

{-| We use [ConsumerProperties] adding information of the broker host, consumer group id, commit strategy and log level-}
consumerProps :: ConsumerProperties
consumerProps = brokersList [BrokerAddress "localhost:9092"]
             <> groupId (ConsumerGroupId "your_consumer_group")
             <> noAutoCommit
             <> logLevel KafkaLogInfo

{-| Create a subscription using [createTopic] functions and is merged using [<>] operator with another subscription created
    when we run [OffsetReset] function to define the Offset strategy -}
createConsumerSubscription :: Subscription
createConsumerSubscription = createTopic "MyFirstTopic"  <> offsetReset Earliest

{-| Create a subscription using [topics] functions which expect a [TopicName] type created passing a topic namex -}
createTopic:: String  -> Subscription
createTopic topic = topics [TopicName topic]

{-| Having a Kafka consumer We use [mapM_] function to start iterating 15 times, polling for messages. -}
consumeMessages :: KafkaConsumer -> IO (Either KafkaError String)
consumeMessages kafkaConsumer = do mapM_ (\_ -> do message <- consumeMessage kafkaConsumer
                                                   SIO.putStrLn $ "Message: " <> show message
                                         )[0 :: Integer .. 15]
                                   _ <- closeConsumer kafkaConsumer
                                   return $ Right "All events processed"

{-| Having a Kafka consumer We use [pollMessage] function passing the consumer and a timeout.
    It return an either of KafkaError or ConsumerRecord.
    We then commit offset using [commitAllOffsets] which return a maybe of Kafka error which in case is filled
    contains the error description.-}
consumeMessage :: KafkaConsumer -> IO (Either KafkaError String)
consumeMessage kafkaConsumer = do eitherConsumerRecord <- pollMessage kafkaConsumer (Timeout 5000)
                                  maybeError <- commitAllOffsets OffsetCommit kafkaConsumer
                                  SIO.putStrLn $ "Offsets: " <> maybe "Committed." show maybeError
                                  eitherResponse <- case eitherConsumerRecord of
                                              Right consumerRecord -> do return $ Right(getConsumerRecordValue consumerRecord)
                                  return eitherResponse

{-| Function to extract the value from the ConsumerRecord -}
getConsumerRecordValue :: ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> String
getConsumerRecordValue(ConsumerRecord _ _ _ _ _ value) = case value of
                                                  Just value -> CH.unpack value
                                                  Nothing ->  "No data find in Customer record"

