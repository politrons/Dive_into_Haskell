![My image](../../../img/feature.png) ![My image](../../../img/apple-music.png)

## Apple Music API

A public API where you can find, filter and append search in the Apple music store

You can see the explanation and implementation of functions [Here](AppleAPI.hs)

### Use

#### Run the server program
```
    main :: IO ()
    main = appleServer
```

#### API description:

The API is based in Restful syntax, where queries use GET method, and arguments of the query are passed as URI params.

* Get product. Return information of one product passed as query param.(/product/incubus)
    ```
       GET: "/product/:product"
    ```

* Get products. Return information of several product passed as query param, using | separator.(/products/metallica|incubus|muse)
    ```
       GET: "/products/:products"
    ```
* Get products by price. Return information of one product passed as query param and filter by min and max price.(/product/ironmaiden/min/0.50/max/2.00)
    ```
      GET: "/product/:product/min/:minPrice/max/:maxPrice"
   ```
* Get product. Return information of one product passed as query param and filter by album.(/product/metallica/album/Death Magnetic)
   ```
      GET: "/product/:product/album/:album"
   ```

* Get product. Return information of one product passed as query param and filter by song.(/product/incubus/song/Wish You Were Here)
   ```
      GET: "/product/:product/song/:song"
   ```

#### Response:

The response is done in Json format and is a result array of all sub-products found for one product.

```json
 "results": [
        {
            "artworkUrl100": "https://is2-ssl.mzstatic.com/image/thumb/Music/v4/f6/30/93/f63093cb-7bfc-7780-bc6f-0d5a79586c57/source/100x100bb.jpg",
            "trackPrice": 1.29,
            "collectionName": "Make Yourself",
            "releaseDate": "1999-10-26T07:00:00Z",
            "primaryGenreName": "ALTERNATIVE",
            "trackName": "Drive",
            "artistName": "Incubus",
            "trackViewUrl": "https://itunes.apple.com/us/album/drive/187454164?i=187454421&uo=4",
            "previewUrl": "https://audio-ssl.itunes.apple.com/apple-assets-us-std-000001/Music/f0/4a/8e/mzm.jsifzrum.aac.p.m4a"
        },
        {
            "artworkUrl100": "https://is4-ssl.mzstatic.com/image/thumb/Music/v4/e4/06/55/e406554f-0656-308f-ecda-aa96925085bd/source/100x100bb.jpg",
            "trackPrice": 1.29,
            "collectionName": "Morning View",
            "releaseDate": "2001-08-21T07:00:00Z",
            "primaryGenreName": "ALTERNATIVE",
            "trackName": "Wish You Were Here",
            "artistName": "Incubus",
            "trackViewUrl": "https://itunes.apple.com/us/album/wish-you-were-here/271792608?i=271792734&uo=4",
            "previewUrl": "https://audio-ssl.itunes.apple.com/apple-assets-us-std-000001/Music/11/06/24/mzm.rhyzaayi.aac.p.m4a"
        }]
```
