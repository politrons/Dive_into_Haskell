![My image](../../../img/feature.png) ![My image](../../../img/file_transfer.png)

## File transfer system

A haskell client/server to connect and transfer files from sender and receiver clients.

You can see the explanation and implementation of functions [Here](FileTransferSystem.hs)

### Use

* Run the server program
    ```.haskell
    main :: IO ()
    main = fileSystem
    ```
* Run the receiver client to wait for files.
    ```.haskell
    main :: IO ()
    main = receiverClient
    ```
* Run the sender client with the path of the file to be transfer.
    ```.haskell
    main :: IO ()
    main = senderClient "some_file.txt"
    ```

* Or you can run a pipeline that it will compose the three main functions:

    ```.haskell
    main :: IO ()
    main = do _ <- forkIO fileSystem
                   threadDelay 2000000
                   _ <- forkIO receiverClient
                   threadDelay 2000000
                   _ <- senderClient "somefile.txt"
                   threadDelay 2000000
    ```
