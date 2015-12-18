module AccessDB (getArtistID,getAlbumInfo,getTrackInfo,getImageURL) where
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import SpotifyDataTypes
import MySqlConnect

{-| The getArtistID function takes a string (The artist Name) and looks
    for any artists in the database with this name (case insensitive).
    If there are any it returns the id, otherwise -1-}
getArtistID :: String -> IO Int
getArtistID artist = do
    conn <- getConnection                                                                                               --Get the database connection
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE LOWER(artistName) = LOWER(?)" [toSql artist]            --Query the Artists table to see if the artist is present
    closeConnection conn                                                                                                --Close the connection
    if (length artistList) > 0 then return (fromSql $ head $ head artistList::Int)                                      --If the artist is present, return their id
    else return (-1)                                                                                                    --Else return -1 (not found)

{-| The getAlbumInfo function takes an Int (The artist ID) and returns
    All of the ID's and Name's of the albums related to that artist.
    These are in the form of a List of Pairs-}
getAlbumInfo:: Int -> IO [(Int, String)]
getAlbumInfo artist = do
    conn <- getConnection                                                                                               --Get the database connection
    albumList <- quickQuery' conn "SELECT idAlbums, albumName FROM Albums WHERE albumArtist = ?" [toSql (artist::Int)]  --Query the album table for the id/names of all artist albums
    closeConnection conn                                                                                                --Close the connection
    return $ extractAlbumInfo albumList                                                                                 --Convert the SQL Responses to pairs via extractAlbumInfo and return

{- The extractAlbumInfo is a helper function which takes a list of list of 
  SqlValues (the returned album data) and returns a list of pairs (the
  format required to return by getAlbumInfo.-}
extractAlbumInfo :: [[SqlValue]] -> [(Int,String)]
extractAlbumInfo [] = []                                                                                                --If the list is empty, return empty list
extractAlbumInfo ((idAlbum:name:_):rest) = ((fromSql idAlbum::Int),fromSql name) : (extractAlbumInfo rest)              --Otherwise take the id and name of each inner list, and convert them from SQLValues to (int,string)

{-| The getTrackInfo function takes an Int (The album ID) and returns
    the Names and Preview Urls of all the Tracks related to that album  
    in the form of a list of pairs-}
getTrackInfo:: Int -> IO [(String,String)]
getTrackInfo album = do
    conn <- getConnection                                                                                               --Get the database connection
    trackList <- quickQuery' conn "SELECT trackName, previewURL FROM Tracks WHERE album = ?" [toSql (album::Int)]       --Query the track table for the names/urls for all of the albums tracks
    closeConnection conn                                                                                                --Close the connection
    return $ extractTrackInfo trackList                                                                                 --Conver the SQL responses to pairs via extractTrackInfo and return

{-The extractTrackInfo is a helper function which takes a list of list of 
  SqlValues (the returned track data) and returns a list of pairs (the
  format required to return by getTrackInfo.-}
extractTrackInfo :: [[SqlValue]] -> [(String,String)]
extractTrackInfo [] = []                                                                                                --If the list is empty, return empty list
extractTrackInfo ((name:url:_):rest) = (fromSql name,fromSql url) : (extractTrackInfo rest)                             --Otherwise take the name and URL from each inner list, and convert from SQLValues to (String,String)

{-| The getImageUrl function takes an Int (the album ID) and returns
    that albums artwork. If no artwork is available, then returns a 
    default image-}
getImageURL :: Int -> IO String
getImageURL album = do
    conn <- getConnection                                                                                               --Get the database connection
    imageList <- quickQuery' conn "SELECT imageURL FROM Images WHERE albumID = ?" [toSql (album::Int)]                  --Query the Image table for the URL for the given album 
    closeConnection conn                                                                                                --Close the database connection
    if(length imageList > 0) then                                                                                       --Check if an image was present
        return $ fromSql $ head $ head imageList                                                                        --If so, take the first row returned (The biggest image), convert from SQLValue to string and return
    else return "https://cdn3.iconfinder.com/data/icons/abstract-1/512/no_image-512.png"                                --Otherwise return default image