{-# LANGUAGE OverloadedStrings #-} --usual string syntax can be used for ByteString, Text, and other variations of string like types

module ParseSpotify (parseArtist) where 

import Network.URI
import Network.HTTP
import Network.HTTP.Conduit
import Network.Connection (TLSSettings (..))
import Data.Maybe
import Data.Either
import Data.Either.Extra
import Database.HDBC as DB
import Database.HDBC.MySQL as MYSQL
import SpotifyDataTypes
import SaveToDB
import MySqlConnect
import Data.Aeson

parseArtist :: String -> IO ()
parseArtist artist = do
    body <-getHTTPbody artist
    let artistInfo = extractArtist body artist
    let artistName = snd artistInfo
    let artistID   = fst artistInfo 
    conn <- getConnection
    artistInDB <- checkArtistInDB artistName conn
    if artistInDB || artistID == "NULLARTIST" then return ()
    else do
        getFullArtist artistID conn
        getArtistAlbums artistID artistName conn
        closeConnection conn

getFullArtist :: String -> MYSQL.Connection -> IO ()
getFullArtist artistID  conn = do
    body <- getHTTPSbody ("https://api.spotify.com/v1/artists/" ++ artistID)
    let decodeResult = decode body :: Maybe FullArtist
    let artistData = fromJust decodeResult
    addArtistToDB  artistData conn


getArtistAlbums :: String -> String -> MYSQL.Connection -> IO ()
getArtistAlbums artistID artistName' conn = do
    body <- getHTTPSbody ("https://api.spotify.com/v1/artists/"++artistID++"/albums?album_type=album")
    let decodeResult = decode body :: Maybe Albums
    let albumList = fromJust decodeResult
    let uniqueAlbums = removeDoubles "" $ albums albumList
    addAlbumToDB uniqueAlbums artistName' conn
    getTracks (uniqueAlbums) conn

getTracks:: [Album] -> MYSQL.Connection -> IO ()
getTracks [] conn = return () 
getTracks (album:albums) conn = do
    let albumNum = albumID album
    body <- getHTTPSbody ("https://api.spotify.com/v1/albums/"++albumNum++"/tracks")
    let decodeResult = decode body :: Maybe Tracks
    let tracklist = fromJust decodeResult
    addTracksToDB (tracks tracklist) albumNum conn
    getTracks albums conn

--getHTTPSbody :: String -> IO Data.ByteString.Lazy.Internal.ByteString
getHTTPSbody url = do
    req <- parseUrl url
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    resp <-withManagerSettings settings $ httpLbs req
    let body = responseBody resp
    return body

--getHTTPbody:: String -> IO Data.ByteString.Lazy.Internal.ByteString
getHTTPbody artist = do
    let uri = fromJust $ parseURI ("http://ws.spotify.com/search/1/artist.json?q=" ++ (urlify artist))
    let req = Request {rqURI=uri, rqMethod=GET, rqHeaders=[], rqBody=""}
    resp <-simpleHTTP req                                                                     
    let body = rspBody $ fromRight resp 
    return body


extractArtist body artist = do
    let decodeResult = fromJust (decode body :: Maybe Info)
    let artistList = artists decodeResult
    if(length artistList > 0) then
         (drop 15 $ href' $ head artistList, name' $ head artistList)
    else ("NULLARTIST","NULLARTIST")

--this function just converts spaces to %20 so that they will work in a url
urlify :: String -> String
urlify [] = []
urlify (x:xs) | x == ' '  = '%':'2':'0':(urlify xs)
              | otherwise = x    :(urlify xs)

--this function goes through a list of albums and removes doubles
-- (the double are always next to eachother) 
removeDoubles:: String -> [Album] -> [Album]
removeDoubles lastPos [] = []
removeDoubles lastPos (x:xs) | lastPos == (albumName x) =    removeDoubles (albumName x) xs   
                             | otherwise                = x:(removeDoubles (albumName x) xs)