{-# LANGUAGE OverloadedStrings #-} --usual string syntax can be used for ByteString, Text, and other variations of string like types

import Network.URI
import Network.HTTP
import Network.HTTP.Conduit
import Network.Connection (TLSSettings (..))
import Data.Maybe
import Data.Either
import Data.Either.Extra
import SpotifyDataTypes
import SpotifyDatabase
import Data.Aeson


searchForArtist :: String -> IO ()
searchForArtist artist = do
    let uri = fromJust $ parseURI ("http://ws.spotify.com/search/1/artist.json?q=" ++ (urlify artist)) --append the users search term to find artist
    let req = Request {rqURI=uri, rqMethod=GET, rqHeaders=[], rqBody=""}
    resp <-simpleHTTP req                                                                     -- build request and send request to spotify api
    let body = rspBody $ fromRight resp                                                       -- extract the body of the message
    let decodeResult = decode body :: Maybe Info                                              -- parse into info object
    let topArtist = head $ artists $ fromJust decodeResult
    let idExtract = drop 15 $ href' topArtist                                                 -- extract artist id which is utilised in FullArtist and Album URL
    let artistName' = name' topArtist
    artistInDB <- checkArtistInDB artistName'
    if artistInDB then print "Artist already in Database!" 
    else do
        getFullArtist idExtract
        getArtistAlbums idExtract artistName'

getFullArtist :: String -> IO ()
getFullArtist artistID = do
    req <- parseUrl ("https://api.spotify.com/v1/artists/" ++ artistID)
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    resp <-withManagerSettings settings $ httpLbs req
    let body = responseBody resp
    let decodeResult = fromJust (decode body :: Maybe FullArtist)
    addArtistToDB  decodeResult
    print "Finished saving Artist information"
    --return (fromJust decodeResult) 


getArtistAlbums :: String -> String -> IO ()
getArtistAlbums artistID artistName' = do
    req <- parseUrl ("https://api.spotify.com/v1/artists/"++artistID++"/albums?album_type=album")
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    resp <-withManagerSettings settings $ httpLbs req
    let body = responseBody resp
    let decodeResult = decode body :: Maybe Albums
    let albumList = fromJust decodeResult
    let uniqueAlbums = removeDoubles "" $ albums albumList
    addAlbumToDB uniqueAlbums artistName'
    print "Finished saving album information"
    getTracks (uniqueAlbums)

getTracks:: [Album] -> IO ()
getTracks [] = return ()
getTracks (album:albums) = do
    let albumNum = albumID album
    req <- parseUrl ("https://api.spotify.com/v1/albums/"++albumNum++"/tracks")
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    resp <-withManagerSettings settings $ httpLbs req
    let body = responseBody resp
    let decodeResult = decode body :: Maybe Tracks
    let tracklist = fromJust decodeResult
    addTracksToDB (tracks tracklist) albumNum
    --print $ trackName $ head $ tracks $ tracklist
    print ("Finished saving track info for album: "++(albumName album))
    getTracks  albums

urlify :: String -> String
urlify [] = []
urlify (x:xs) | x == ' '  = '%':'2':'0':(urlify xs)
              | otherwise = x    :(urlify xs)

removeDoubles:: String -> [Album] -> [Album]
removeDoubles lastPos [] = []
removeDoubles lastPos (x:xs) | lastPos == (albumName x) =    removeDoubles (albumName x) xs   
                             | otherwise                = x:(removeDoubles (albumName x) xs)