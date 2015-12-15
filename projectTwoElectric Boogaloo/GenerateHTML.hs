module GenerateHTML (createPage) where

import AccessDB
import ParseSpotify
import Data.List.Split

createPage:: String -> IO String
createPage "favicon.ico" = return "" --stops requests for small icon in tab
createPage artist = do
    fstCheck   <- getArtistID artist
    if fstCheck == (-1) then do
      parseArtist artist
      sndCheck <- getArtistID artist
      if sndCheck == (-1) then do
        return "This artist is unavailable on spotify"
      else  pageBuilder artist sndCheck
    else  pageBuilder artist fstCheck
      
pageBuilder:: String -> Int -> IO String
pageBuilder artist idArtist = do
  albumPairs <- getAlbumInfo idArtist
  albumText  <- outputAlbums albumPairs
  htmlHead   <- readFile "header.html" 
  let html = unlines ["<html>",htmlHead,
                      "<p><center><h1>",artist," Album Preview","</h1></center></p>",
                      "<div id=\"accordion\">",albumText,"</body></html>"]
  return html

outputAlbums:: [(Int,String)] -> IO String
outputAlbums [] = return ""
outputAlbums (album:albums) = do 
    trackPairs <- getTrackInfo (fst album)
    remainingAlbums <- outputAlbums albums
    albumArtwork <- getImageURL (fst album)
    return $ unlines ["<h3 class=\"album\">","<div class=\"innerDiv\">",
                      "<img src=\"",albumArtwork,"\" height=\"20%\">",(snd album),
                      "</div></h3><div><center><table cellspacing=\"15pt\" cellpadding=\"15pt\">",
                      (outputTracks trackPairs 1),"</table></center></div>",remainingAlbums]

outputTracks:: [(String,String)] -> Int -> String
outputTracks [] _ =  "</tr>"
outputTracks (track:tracks) rowCount | rowCount == 1 = "<tr><td>"++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 2)
outputTracks (track:tracks) rowCount | rowCount == 2 = "<td>"    ++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 3)
outputTracks (track:tracks) rowCount | rowCount == 3 = "<td>"    ++(insideTD track) ++ "</td></tr>" ++ (outputTracks tracks 1) 

insideTD :: (String,String) -> String
insideTD track = unlines ["<p><center><h4>",
                           (fst track),
                           "</h4></p>",
                           "<audio controls> <source src=\"",(snd track),"\" type=\"audio/mpeg\">",
                           "</center></audio>"]
