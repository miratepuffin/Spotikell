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
import Control.Exception
import Data.Aeson
import qualified Control.Exception
import Control.Applicative
import Control.Monad
import Network.HTTP.Types.Status   (statusCode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as C

{-| The parseArtist method takes an artist name as a string,
    and looks the artist up via the spotify search API. 
    If found, if gathers all the information about them, their
    albums and the tracks of those albums, storing the information
    in the database specified by MySQLConnect.hs
    -}
parseArtist :: String -> IO ()
parseArtist artist = do
    body <-getHTTPSbody ("http://ws.spotify.com/search/1/artist.json?q=\"" ++ (urlify artist)++"\"")     --Query the Spotify rest api for Artists with the given name
    let artistInfo = extractArtist body                                                                  --Extract the Name and spotify ID of the closest match
    let artistName = snd artistInfo
    let artistID   = fst artistInfo
    conn <- getConnection                                                                                --Open a connection to the database
    artistInDB <- checkArtistInDB artistName conn                                                        --See if the artist is already in the database
    if artistInDB || artistID == "NULLARTIST" then return ()                                             --If they are in the DB already or the search was empty return nothing
    else do                                                                                              --Otherwise
        getFullArtist artistID conn                                                                      --Get/store the full artist information
        getArtistAlbums artistID artistName conn                                                         --Get/store the artist album/track information
        closeConnection conn                                                                             --Commit the changes and close the DB connection
                                                                                                         --The same connection object is passed around so if an error occurs no changes are commited

{- The getFullArtist method takes a spotify artistID and a connection object.
   It queries the spotify artist RestAPI for the given ID, converts the returned
   JSON into a FullArtist object and saves it in the database  -}
getFullArtist :: String -> MYSQL.Connection -> IO ()
getFullArtist artistID  conn = do
    body <- getHTTPSbody ("https://api.spotify.com/v1/artists/" ++ artistID)                             --Contact the Spotify artist Api for the given spotify ID
    let decodeResult = decode body :: Maybe FullArtist                                                   --Convert the returned JSON to a FullArtist object
    let artistData = fromJust decodeResult
    addArtistToDB  artistData conn                                                                       --Pass the object to SaveTODB.hs to handle adding to database


getArtistAlbums :: String -> String -> MYSQL.Connection -> IO ()
getArtistAlbums artistID artistName' conn = do
    body <- getHTTPSbody ("https://api.spotify.com/v1/artists/"++artistID++"/albums?album_type=album")  --Contact spotify album API with the given artist ID
    let decodeResult = decode body :: Maybe Albums                                                      --Convert the returned Json to an Albums object (containing list of Albums)
    let albumList = fromJust decodeResult
    let uniqueAlbums = albums albumList                                                                 --Extract album list from Albums object
    addAlbumToDB uniqueAlbums artistName' conn                                                          --Pass Albums list to SaveToDB.hs to handle saving albums to database.
    getTracks (uniqueAlbums) conn                                                                       --Pass Albums list to getTracks to download and store the songs from each album


getTracks:: [Album] -> MYSQL.Connection -> IO ()
getTracks [] conn = return ()                                                                           --First check if the list is empty, to see if there are remaining albums to process
getTracks (album:albums) conn = do
    let albumNum = albumID album                                                                        --Extract the id of the given album
    body <- getHTTPSbody ("https://api.spotify.com/v1/albums/"++albumNum++"/tracks")                    --Contact the spotify track API with the given album ID
    let decodeResult = decode body :: Maybe Tracks                                                      --Convert the returned JSON into a Tracks object (containing a list of Tracks)
    trackSuccessCheck decodeResult albumNum conn                                                        --Make sure the track data is not incorrect (many trash albums left in API) and save to DB if safe
    getTracks albums conn                                                                               --Call the method again to parse the rest of the albums

trackSuccessCheck:: Maybe Tracks -> String -> MYSQL.Connection -> IO()
trackSuccessCheck (Just tracklist) albumNum conn = addTracksToDB (tracks tracklist) albumNum conn       --If the Parse from JSON to Tracks was successful, pass the Tracks to SaveToDB.hs for saving
trackSuccessCheck Nothing albumNum conn           = print ("Broken Album: "++albumNum)                  --Otherwise print the id of the broken album and move on


extractArtist:: BL.ByteString -> (String,String)
extractArtist body = do
    let decodeResult = fromJust (decode body :: Maybe Info)                                             --Convert the JSON to an Info object
    let artistList = artists decodeResult                                                               --Extract the list of Basic Artists for the Info object
    if(length artistList > 0) then                                                                      --If there are Artists present 
         (drop 15 $ href' $ head artistList, name' $ head artistList)                                   --Retreve the Spotify Id of the artist by trimming the spotify href, and the name
    else ("NULLARTIST","NULLARTIST")                                                                    --Otherwise Return NULLARTIST

urlify :: String -> String
urlify [] = []                                                                                          --Check if there are still characters to process
urlify (x:xs) | x == ' '  = '%':'2':'0':(urlify xs)                                                     --If the character is a space convert it into %20 for insertion into the api call
              | otherwise = x    :(urlify xs)                                                           --otherwise just return the character

getHTTPSbody :: String -> IO BL.ByteString
getHTTPSbody url = do
    req <- parseUrl url                                                                                 --Check that the URL is in a correct format
    manager <- newManager tlsManagerSettings                                                            --Build a manager for the request (specifies properties which allow for HTTPS connections)
    resp <- catch (Just <$> httpLbs req manager) someException                                          --Execture the query, catching any HTTPEXceptions
    case resp of                                                                                        
        Nothing -> return (C.pack "Error")                                                              --If there was an error return the string "Error"
        otherwise -> return $ responseBody $ fromJust resp                                              --Otherwise return the body of the 

someException :: HttpException -> IO (Maybe a)
someException (StatusCodeException s _ _) = 
  putStrLn ((show . statusCode $ s) ++ " Exception has occured. Check the URL!") >> return Nothing   --Prints the Broken url to terminal to be checked later