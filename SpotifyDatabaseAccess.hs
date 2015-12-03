module SpotifyDatabaseAccess (createPageTest) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import SpotifyDataTypes

mySQLInfo :: MySQLConnectInfo
mySQLInfo = defaultMySQLConnectInfo {
  mysqlHost = "localhost",
  mysqlDatabase = "spotify",
  mysqlUser = "root",
  mysqlPassword = "1234",
  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" 
}
getConnection :: IO Connection
getConnection = do 
  conn <- connectMySQL mySQLInfo
  return conn

getArtistID :: String -> IO Int
getArtistID artist = do
    conn <- getConnection
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql artist]
    return (fromSql $ head $ head artistList::Int)

getAlbumInfo:: Int -> IO [(Int, String)]
getAlbumInfo artist = do
    conn <- getConnection
    albumList <- quickQuery' conn "SELECT idAlbums, albumName FROM Albums WHERE albumArtist = ?" [toSql (artist::Int)]
    return $ extractAlbumInfo albumList

extractAlbumInfo :: [[SqlValue]] -> [(Int,String)]
extractAlbumInfo [] = []
extractAlbumInfo ((idAlbum:name:_):rest) = ((fromSql idAlbum::Int),fromSql name) : (extractAlbumInfo rest)

getTrackInfo:: Int -> IO [(String,String)]
getTrackInfo album = do
    conn <- getConnection
    trackList <- quickQuery' conn "SELECT trackName, previewURL FROM Tracks WHERE album = ?" [toSql (album::Int)]
    return $ extractTrackInfo trackList

extractTrackInfo :: [[SqlValue]] -> [(String,String)]
extractTrackInfo [] = [] 
extractTrackInfo ((name:url:_):rest) = (fromSql name,fromSql url) : (extractTrackInfo rest)

createPageTest:: String -> IO ()
createPageTest artist = do
    idArtist <- getArtistID artist
    albumPairs <- getAlbumInfo idArtist
    trackPairs <- getTrackInfo (fst $ head albumPairs) 
    let html = unlines ["<html><head/><body>",
                        "<p><h1>",
                        (snd $ head albumPairs),
                        "</h1></P>",
                        "<p><h2>",
                        (fst $ head trackPairs),
                        "</h2></p>",
                        "<audio controls> <source src=\"",
                        (snd $ head trackPairs),
                        "\" type=\"audio/mpeg\"",
                        "</body></html>"]
    writeFile "test.html" html