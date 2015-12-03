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

getImageURL :: Int -> IO String
getImageURL album = do
    conn <- getConnection
    imageList <- quickQuery' conn "SELECT imageURL FROM Images WHERE albumID = ?" [toSql (album::Int)]
    return $ fromSql $ head $ head imageList

createPageTest:: String -> IO ()
createPageTest artist = do
    idArtist   <- getArtistID artist
    albumPairs <- getAlbumInfo idArtist
    albumText  <- outputAlbums albumPairs
    let html = unlines ["<html><head/><body>",
                        albumText,
                        "</body></html>"]
    writeFile (artist++".html") html

outputAlbums:: [(Int,String)] -> IO String
outputAlbums [] = return ""
outputAlbums (album:albums) = do 
    trackPairs <- getTrackInfo (fst album)
    remainingAlbums <- outputAlbums albums
    albumArtwork <- getImageURL (fst album)
    return $ unlines ["<p><h1><center>",
                      (snd album),
                      "</h1></center></P>",
                      "<p><center><img src=\"",
                      albumArtwork,
                      "\" alt=\"Album image\">",
                      "</h1></center></P>",
                      (outputTracks trackPairs),
                      remainingAlbums]

outputTracks:: [(String,String)] -> String
outputTracks [] = ""
outputTracks (track:tracks) = unlines ["<p><h2>",
                                       (fst track),
                                       "</h2></p>",
                                       "<audio controls> <source src=\"",
                                       (snd track),
                                       "\" type=\"audio/mpeg\">",
                                       "Your browser does not support the audio element.",
                                       "</audio>",
                                       (outputTracks tracks)]
