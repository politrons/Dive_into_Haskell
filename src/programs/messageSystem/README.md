![My image](../../../img/feature.png) ![My image](../../../img/chat.svg)

## Message system

A haskell server to connect and communicate with other clients using ```Telnet``.`

You can see the implementation [Here](MessageSystem.hs)

### Use

* Run the main program
```.haskell
main :: IO ()
main = messageSystem
```
* Connect by telnet to the port ```1981```
```
telnet localhost 1981
```
* Add your username
```
Welcome to Politrons message system. How shall I call you?
```
* Start communicating
```
Human Tron logged
```