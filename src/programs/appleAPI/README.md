![My image](../../../img/feature.png) ![My image](../../../img/apple-music.png)

## Apple Music API

A public API where you can find, filter and append search in the Apple music store

You can see the explanation and implementation of functions [Here](AppleAPI.hs)

### Use

* Run the server program
    ```.haskell
    main :: IO ()
    main = appleServer
    ```
* API description:

    * Get product. Return information of one product passed as query param
        ```
            GET: "/product/:product"
        ```
    * Get products. Return information of several product passed as query param, using | separator. (/products/metallica|incubus|muse)
        ```
            GET: "/products/:products"
        ```
    * Get products by price. Return information of one product passed as query param and filter by min and max price.
        ```
            GET: "/product/:product/min/:minPrice/max/:maxPrice"
        ```
    * Get product. Return information of one product passed as query param and filter by album
        ```
            GET: "/product/:product/album/:album"
        ```

