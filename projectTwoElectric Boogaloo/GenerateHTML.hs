module GenerateHTML (createPage) where

import AccessDB
import ParseSpotify
import Data.List.Split

{-| The create page function takes a string argument consistsing of an artists name.
    It checks if the artist is in the database and if not attempts to add them
    If this is successful, the artist page is build, if not the NoArtist page is returned
    (found in the HTML folder)-}
createPage:: String -> IO String
createPage "favicon.ico" = return ""                                                      --Firstly check if it is the icon being requested. As there is not one this requested is ignored
createPage artist = do
    fstCheck   <- getArtistID artist                                                      --Check if the artist is already in the database, as no need to parse the data if they are
    if fstCheck == (-1) then do                                                           --If the first check returns -1 they are not present
      parseArtist artist                                                                  --So the parse artist function is called to enter them
      sndCheck <- getArtistID artist                                                      --A second check is performed to see if they are now present
      if sndCheck == (-1) then do                                                         --If they are once again not in the database, it means that they are not in the spotify API
        readFile "HTML/NoArtist.html"                                                     --So the NoArtist page is returned instead
      else  pageBuilder artist sndCheck                                                   --If They are present the first or second time the build page function is called
    else  pageBuilder artist fstCheck

{- pageBuilder is the overarching function in the build process,
   it gets the inital information and builds the overall structure of the page -}
pageBuilder:: String -> Int -> IO String
pageBuilder artist idArtist = do
  albumPairs <- getAlbumInfo idArtist                                                     --Get the album ID and Name for all albums from the given artist
  albumText  <- outputAlbums albumPairs                                                   --Generate all of the collapsable div elements (one for each album)
  htmlHead   <- readFile "HTML/header.html"                                               --Read in the Head file which specifies the Jquery used for the collapsing/opening of the divs
  let html = unlines ["<html>",htmlHead,                                                  --Open up the HTML document and add the head
                      "<p><center><h1>",artist," Album Preview","</h1></center></p>",     --Add the title including the artist name (body already opened in header to add search bar)
                      "<div id=\"accordion\">",albumText,"</div></body></html>"]          --Create the Div containing all the albums and and end the body/document 
  return html                                                                             --Return the document which will then be sent to the user

{- outputAlbums takes the list of album ID's and name's in the form of a pair and 
   builds the inner divs containg the album information, until the data is exhausted -}
outputAlbums:: [(Int,String)] -> IO String
outputAlbums [] = return ""                                                                          --Firstly check if the list is empty and return empty string if so
outputAlbums (album:albums) = do
    trackPairs <- getTrackInfo (fst album)                                                           --Get the names and preview URL's for the albums tracks
    remainingAlbums <- outputAlbums albums                                                           --Recurcivly call the method on the remaining albums (as they go at the end of the returned text)
    albumArtwork <- getImageURL (fst album)                                                          --Get the Url for the album artwork
    return $ unlines ["<h3 class=\"album\">","<div class=\"innerDiv\">",                             --Create the header div (closed div view) for the album
                      "<img src=\"",albumArtwork,"\" height=\"20%\" >",(snd album),                  --Add the album artwork and name to the header
                      "</div></h3><div><center><table cellspacing=\"15pt\" cellpadding=\"15pt\">",   --Close the header div and open the content div. As the tracks are stored in a table, open that
                      (outputTracks trackPairs 1),"</table></center></div>",remainingAlbums]         --Generate all the table rows containing track previews, close the table/content div
                                                                                                     --Then add the rest of the album divs. This is then returned.

{- output tracks takes the list of track ID's/URLs and a position int. 
   Based on this is it decides weather a new row is requried (allowing for
   3 tracks per row in the table. The format of the track data is done in the
   insideTD function -}
outputTracks:: [(String,String)] -> Int -> String
outputTracks [] _ =  "</tr>"                                                                                                    --Firstly check if the list is empty, if it is return an end/empty td tag
outputTracks (track:tracks) rowCount | rowCount == 1 = "<tr><td>"++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 2)  --If it is the start of a row open a new row and place the track info inside a td
outputTracks (track:tracks) rowCount | rowCount == 2 = "<td>"    ++(insideTD track) ++ "</td>"      ++ (outputTracks tracks 3)  --If it is the middle of a row just add the info into a td
outputTracks (track:tracks) rowCount | rowCount == 3 = "<td>"    ++(insideTD track) ++ "</td></tr>" ++ (outputTracks tracks 1)  --If it is the end of a row add the info to a td and close the row

{- insideTD is a helper function which takes a single track pair and formats
   it, to be placed inside the table data elements of the above function-}
insideTD :: (String,String) -> String
insideTD track = unlines ["<center><p><h4>",(fst track),"</h4></p>",                                                --Add the Track name as a header within <p> tags (which adds a new line when closed) 
                          "<audio controls> <source src=\"",(snd track),"\" type=\"audio/mpeg\"></audio></center>"] --Add the preview URL in an Audio control block, which allows the user to listen 
