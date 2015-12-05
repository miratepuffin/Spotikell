module SpotifyDatabaseAccess (createPageTest) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Data.List.Split
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
    let html = unlines ["<html>",
                        createHeader,
                        "<body>",
                        "<p><center><h1>",artist," Album Preview","</h1></center></p>",
                        "<div id=\"accordion\">",
                        albumText,
                        "</body></html>"]

    let htmlRedux = rmvBlankLines $ splitOn "\n" html
    writeFile (artist++".html") htmlRedux

createHeader :: String
createHeader = unlines ["<head>",
  "<meta charset=\"utf-8\">",
  "<title>CHANGE CHANGE CHAGE</title>",
  "<link rel= \"stylesheet\" href=\"jquery-ui.css\">",
  "<script src=\"http://code.jquery.com/jquery-1.10.2.js\"></script>",
  "<script src=\"http://code.jquery.com/ui/1.11.4/jquery-ui.js\"></script>",
  --"<link rel=\"stylesheet\" href=\"/resources/demos/style.css\">",
  "<script>",
  "$(function() {",
  "$( \"#accordion\" ).accordion();",
  "$(\"#accordion\").show().accordion({autoHeight: false});",
  "$(\"#accordion div\").css({ 'height': 'auto' });",
  "$( \"#accordion\" ).accordion({collapsible: true});",
  "$( \"#accordion\" ).accordion({active: false});",
  "});",
  "</script>",
  "</head>"] 
outputAlbums:: [(Int,String)] -> IO String
outputAlbums [] = return ""
outputAlbums (album:albums) = do 
    trackPairs <- getTrackInfo (fst album)
    remainingAlbums <- outputAlbums albums
    albumArtwork <- getImageURL (fst album)
    return $ unlines ["<h3 class=\"album\">",
                      "<div class=\"innerDiv\"><img src=\"",albumArtwork,"\" height=\"20%\">",
                      (snd album),
                      "</div></h3><div><center><table cellspacing=\"100pt\" cellpadding=\"40pt\">",
                      (outputTracks trackPairs 1),
                      "</table></center></div>",
                      remainingAlbums]

--outputTracks:: [(String,String)] -> String
--outputTracks [] = ""
--outputTracks (track:tracks) = unlines ["<p><h2>",
--                                       (fst track),
--                                       "</h2></p>",
--                                       "<audio controls> <source src=\"",(snd track),"\" type=\"audio/mpeg\">",
--                                       "</audio>",
--                                       (outputTracks tracks)]

outputTracks:: [(String,String)] -> Int -> String
outputTracks [] _ =  "</tr>"
outputTracks (track:tracks) rowCount | rowCount == 1 = "<tr><td>"++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 2)
outputTracks (track:tracks) rowCount | rowCount == 2 = "<td>"    ++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 3)
outputTracks (track:tracks) rowCount | rowCount == 3 = "<td>"    ++(insideTD track) ++ "</td></tr>" ++ (outputTracks tracks 1) 


insideTD track = unlines ["<p><center><h4>",
                           (fst track),
                           "</h4></p>",
                           "<audio controls> <source src=\"",(snd track),"\" type=\"audio/mpeg\">",
                           "</center></audio>"]

rmvBlankLines :: [String] -> String
rmvBlankLines lines = unlines $ filter notBlank lines 
                        where notBlank a = not ((a == "")|| (a =="\r")|| (a =="\n"))
