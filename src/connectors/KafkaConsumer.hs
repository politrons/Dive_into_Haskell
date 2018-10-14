module KafkaConsumer where

{-| Before start codgin remember that you need install in your system [librdkafka]
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
             <> groupId (ConsumerGroupId "consumer14_group_13")
             <> noAutoCommit
             <> logLevel KafkaLogInfo

{-| Create a subscription using [createTopic] functions and is merged using [<>] operator with another subscription created
    when we run [OffsetReset] function to define the Offset strategy -}
createConsumerSubscription :: Subscription
createConsumerSubscription = createTopic "MyFirstTopic"  <> offsetReset Earliest

{-| Create a subscription using [topics] functions which expect a [TopicName] type created passing a topic namex -}
createTopic:: String  -> Subscription
createTopic topic = topics [TopicName topic]

{-| Having a Kafka consumer we start iterating 10 times per messages-}
consumeMessages :: KafkaConsumer -> IO (Either KafkaError String)
consumeMessages kafkaConsumer = do mapM_ (\_ -> do message <- processMsg kafkaConsumer
                                                   SIO.putStrLn $ "Message: " <> show message
                                         )[0 :: Integer .. 15]
                                   return $ Right "All events processed"

processMsg :: KafkaConsumer -> IO (Either KafkaError String)
processMsg kafka = do eitherConsumerRecord <- pollMessage kafka (Timeout 5000)
                      err <- commitAllOffsets OffsetCommit kafka
                      SIO.putStrLn $ "Offsets: " <> maybe "Committed." show err
                      eitherResponse <- case eitherConsumerRecord of
                                              Left kafkaError -> do return $ Left(kafkaError)
                                              Right consumerRecord -> do return $ Right(getConsumerRecordValue consumerRecord)
                      return eitherResponse

getConsumerRecordValue :: ConsumerRecord (Maybe ByteString) (Maybe ByteString) -> String
getConsumerRecordValue(ConsumerRecord _ _ _ _ _ value) = case value of
                                                  Just value -> CH.unpack value
                                                  Nothing ->  "No data find in Customer record"




--getByteStringPayload :: BS.ByteString -> String
--getByteStringPayload(BS.ByteString payload _ _) = payload

--runConsumer :: IO ()
--runConsumer = do
--    print $ cpLogLevel consumerProps
--    res <- bracket mkConsumer clConsumer runHandler
--    print res
--    where
--      mkConsumer = newConsumer consumerProps createConsumerSubscription
--      clConsumer (Left err) = return (Left err)
--      clConsumer (Right kc) = (maybe (Right ()) Left) <$> closeConsumer kc
--      runHandler (Left err) = return (Left err)
--      runHandler (Right kc) = processMessages kc

-------------------------------------------------------------------
--processMessages :: KafkaConsumer -> IO (Either KafkaError ())
--processMessages kafka = do
--    mapM_ (\_ -> do
--                   msg1 <- pollMessage kafka (Timeout 1000)
--                   putStrLn $ "Message: " <> show msg1
--                   err <- commitAllOffsets OffsetCommit kafka
--                   putStrLn $ "Offsets: " <> maybe "Committed." show err
--          ) [0 :: Integer .. 10]
--    return $ Right ()

--printingOffsetCallback :: KafkaConsumer -> KafkaError -> [TopicPartition] -> IO ()
--printingOffsetCallback _ e ps = do
----    print ("Offsets callback:" ++ show e)
--    mapM_ (print . (tpTopicName &&& tpPartition &&& tpOffset)) ps