module AccessDB (getArtistID,getAlbumInfo,getTrackInfo,getImageURL) where
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import SpotifyDataTypes
import MySqlConnect

getArtistID :: String -> IO Int
getArtistID artist = do
    conn <- getConnection
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql artist]
    closeConnection conn
    if (length artistList) > 0 then return (fromSql $ head $ head artistList::Int)
    else return (-1)

getAlbumInfo:: Int -> IO [(Int, String)]
getAlbumInfo artist = do
    conn <- getConnection
    albumList <- quickQuery' conn "SELECT idAlbums, albumName FROM Albums WHERE albumArtist = ?" [toSql (artist::Int)]
    closeConnection conn
    return $ extractAlbumInfo albumList

extractAlbumInfo :: [[SqlValue]] -> [(Int,String)]
extractAlbumInfo [] = []
extractAlbumInfo ((idAlbum:name:_):rest) = ((fromSql idAlbum::Int),fromSql name) : (extractAlbumInfo rest)

getTrackInfo:: Int -> IO [(String,String)]
getTrackInfo album = do
    conn <- getConnection
    trackList <- quickQuery' conn "SELECT trackName, previewURL FROM Tracks WHERE album = ?" [toSql (album::Int)]
    closeConnection conn
    return $ extractTrackInfo trackList

extractTrackInfo :: [[SqlValue]] -> [(String,String)]
extractTrackInfo [] = [] 
extractTrackInfo ((name:url:_):rest) = (fromSql name,fromSql url) : (extractTrackInfo rest)

getImageURL :: Int -> IO String
getImageURL album = do
    conn <- getConnection
    imageList <- quickQuery' conn "SELECT imageURL FROM Images WHERE albumID = ?" [toSql (album::Int)]
    closeConnection conn
    return $ fromSql $ head $ head imageList
