{-# LANGUAGE OverloadedStrings #-} --usual string syntax can be used for ByteString, Text, and other variations of string like types

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
import SpotifyDatabase
import SpotifyDatabaseCreate
import Data.Aeson

searchForArtist :: String -> IO ()
searchForArtist artist = do
    --search for the artist 
    body <-getHTTPbody artist
    -- convert the returned json data into an Info object                                                       
    let decodeResult = decode body :: Maybe Info
    --extract the id of the top search result                                              
    let topArtist = head $ artists $ fromJust decodeResult
    --cut off the spotify application specific address
    let idExtract = drop 15 $ href' topArtist
    -- get the name of the artist as stored on the API                                                 
    let artistName' = name' topArtist
    --create the connection object which will be parsed through all of the add methods
    conn <- getConnection
    --check if the artist is already saved in the database
    artistInDB <- checkArtistInDB artistName' conn
    -- if it does we inform the user
    if artistInDB then print "Artist already in Database!" 
    else do
        --if if doesn't we start the process of downloading and saving all of the artist information
        getFullArtist idExtract conn
        getArtistAlbums idExtract artistName' conn
        -- we commit and close the connection. We do this last as if errors  
        -- are thrown at any point we don't get semi complete data
        closeConnection conn

getFullArtist :: String -> MYSQL.Connection -> IO ()
getFullArtist artistID  conn = do
    --query the api for the full artist information
    body <- getHTTPSbody ("https://api.spotify.com/v1/artists/" ++ artistID)
    --convert response to Full Artist object
    let decodeResult = decode body :: Maybe FullArtist
    let artistData = fromJust decodeResult
    --add the artist info to the database
    addArtistToDB  artistData conn
    print "Finished saving Artist information"


getArtistAlbums :: String -> String -> MYSQL.Connection -> IO ()
getArtistAlbums artistID artistName' conn = do
    --query the api for the albums by the given artist (full albums only)
    body <- getHTTPSbody ("https://api.spotify.com/v1/artists/"++artistID++"/albums?album_type=album")
    --convert the response to Albums object 
    let decodeResult = decode body :: Maybe Albums
    let albumList = fromJust decodeResult
    --remove doubles that are returned for some reason
    let uniqueAlbums = removeDoubles "" $ albums albumList
    --store albums in database
    addAlbumToDB uniqueAlbums artistName' conn
    print "Finished saving album information"
    --retrieve the tracks for all of the albums
    getTracks (uniqueAlbums) conn

getTracks:: [Album] -> MYSQL.Connection -> IO ()
getTracks [] conn = return () 
getTracks (album:albums) conn = do
    let albumNum = albumID album
    --query the api for the list of tracks for the head of the list
    body <- getHTTPSbody ("https://api.spotify.com/v1/albums/"++albumNum++"/tracks")
    --convert the response to a Tracks object
    let decodeResult = decode body :: Maybe Tracks
    let tracklist = fromJust decodeResult
    --store the tracks in the database
    addTracksToDB (tracks tracklist) albumNum conn
    print ("Finished saving track info for album: "++(albumName album))
    --recall the function until there are no albums remaining 
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