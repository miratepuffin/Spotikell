module SpotifyCreatePage (createPage) where

import SpotifyDatabaseAccess
import Data.List.Split

createPage:: String -> IO ()
createPage artist = do
    idArtist   <- getArtistID artist
    albumPairs <- getAlbumInfo idArtist
    albumText  <- outputAlbums albumPairs
    let html = unlines ["<html>",
                        (createHeader artist),
                        "<body>",
                        "<p><center><h1>",artist," Album Preview","</h1></center></p>",
                        "<div id=\"accordion\">",
                        albumText,
                        "</body></html>"]
    let htmlRedux = rmvBlankLines $ splitOn "\n" html
    writeFile ("CreatedHTML/"++artist++".html") htmlRedux

createHeader :: String -> String
createHeader artist = unlines ["<head>",
  "<meta charset=\"utf-8\">",
  "<title>"++artist++" Preview</title>",
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
                      "</div></h3><div><center><table cellspacing=\"15pt\" cellpadding=\"15pt\">",
                      (outputTracks trackPairs 1),
                      "</table></center></div>",
                      remainingAlbums]

outputTracks:: [(String,String)] -> Int -> String
outputTracks [] _ =  "</tr>"
outputTracks (track:tracks) rowCount | rowCount == 1 = "<tr><td>"++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 2)
outputTracks (track:tracks) rowCount | rowCount == 2 = "<td>"    ++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 3)
outputTracks (track:tracks) rowCount | rowCount == 3 = "<td>"    ++(insideTD track) ++ "</td></tr>" ++ (outputTracks tracks 1) 

insideTD :: String
insideTD track = unlines ["<p><center><h4>",
                           (fst track),
                           "</h4></p>",
                           "<audio controls> <source src=\"",(snd track),"\" type=\"audio/mpeg\">",
                           "</center></audio>"]

rmvBlankLines :: [String] -> String
rmvBlankLines lines = unlines $ filter notBlank lines 
                        where notBlank a = not ((a == "")|| (a =="\r")|| (a =="\n"))
