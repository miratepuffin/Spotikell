{-# LANGUAGE OverloadedStrings #-} --usual string syntax can be used for ByteString, Text, and other variations of string like types

import Network.URI
import Network.HTTP
import Network.HTTP.Conduit
import Network.Connection (TLSSettings (..))
import Data.Maybe
import Data.Either
import Data.Either.Extra
import SpotifyDataTypes
import Data.Aeson

searchForArtist :: String -> IO Tracks
searchForArtist artist = do
    let uri = fromJust $ parseURI ("http://ws.spotify.com/search/1/artist.json?q=" ++ (urlify artist)) --append the users search term to find artist
    let req = Request {rqURI=uri, rqMethod=GET, rqHeaders=[], rqBody=""}
    resp <-simpleHTTP req                                                                     -- build request and send request to spotify api
    let body = rspBody $ fromRight resp                                                       -- extract the body of the message
    let decodeResult = decode body :: Maybe Info                                              -- parse into info object
    let idExtract = drop 15 $ href' $ head $ artists $ fromJust decodeResult                  -- extract artist id which is utilised in FullArtist and Album URL
    getFullArtist idExtract
    getArtistAlbums idExtract

getFullArtist :: String -> IO FullArtist
getFullArtist artistID = do
    req <- parseUrl ("https://api.spotify.com/v1/artists/" ++ artistID)
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    resp <-withManagerSettings settings $ httpLbs req
    let body = responseBody resp
    let decodeResult = decode body :: Maybe FullArtist
    return (fromJust decodeResult) 

getArtistAlbums :: String -> IO Tracks
getArtistAlbums artistID = do
    req <- parseUrl ("https://api.spotify.com/v1/artists/"++artistID++"/albums?album_type=album")
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    resp <-withManagerSettings settings $ httpLbs req
    let body = responseBody resp
    let decodeResult = decode body :: Maybe Albums
    let albumList = fromJust decodeResult
    let uniqueAlbums = removeDoubles "" $ albums albumList
    --return albumList
    getTracks (uniqueAlbums)

getTracks:: [Album] -> IO Tracks
getTracks [] = return (Tracks [])
getTracks (album:albums) = do
    let albumNum = albumID album
    print ("album: "++(albumName album))
    req <- parseUrl ("https://api.spotify.com/v1/albums/"++albumNum++"/tracks")
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    resp <-withManagerSettings settings $ httpLbs req
    let body = responseBody resp
    let decodeResult = decode body :: Maybe Tracks
    let tracklist = fromJust decodeResult
    print $ trackName $ head $ tracks $ tracklist
    getTracks  albums
    return tracklist

urlify :: String -> String
urlify [] = []
urlify (x:xs) | x == ' '  = '%':'2':'0':(urlify xs)
              | otherwise = x    :(urlify xs)

removeDoubles:: String -> [Album] -> [Album]
removeDoubles lastPos [] = []
removeDoubles lastPos (x:xs) | lastPos == (albumName x) =    removeDoubles (albumName x) xs   
                             | otherwise                = x:(removeDoubles (albumName x) xs)