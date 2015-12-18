module SaveToDB (checkArtistInDB,addArtistToDB, addAlbumToDB, addTracksToDB) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import MySqlConnect
import SpotifyDataTypes

{-| The checkArtistInDB function takes an artist name as a string
    and a Connection object. It then returns a boolean denoting 
    weather of not the artist is contained within the artists table-}
checkArtistInDB:: String -> Connection -> IO Bool
checkArtistInDB stringID conn = do
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql stringID]                            --Query the Artist table for any artists with the given name
    if (length artistList) == 0 then return False                                                                           --If there are none return false 
    else return True                                                                                                        --otherwise true
    
{-| The addArtistToDB function Takes a FullArtist Object and a connection
    object. It adds the FullArtist to the database, and calls teh addGenreToDB
    function to deal with the artists genres-}
addArtistToDB:: FullArtist -> Connection -> IO ()
addArtistToDB artist conn = do
    let stmt = "INSERT INTO Artists (artistName,artistSpotifyID,artistFollowers,artistPopularity) VALUES (?, ?, ?, ?)"      --Create the startment for inserting all data from the FullArtist into the database
    run conn stmt [toSql (name artist),                                                                                     --run the statement extracting the required info from the FullArtist and converting to SQL Value
                   toSql (identifier artist),
                   toSql ((followers artist)::Int),
                   toSql ((popularity artist)::Double)]
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql (name artist)]                       --Query the Artist Table to get the ID of the saved artist
    addGenreToDB (genres artist) (fromSql $ head $ head $ artistList ::Int) conn                                            --Add the artists genres to the database (parsing the Database id extracted from the DB query)

{-The addGenreToDB function takes a list of genres (Strings), the position of the
  artist in the Artist Table and a connection object. It checks if the genre already
  exists within the database, if it does it adds the genreID/artistID to the 
  ArtistGenres table. If it doesn't then it first adds it to the genre table, 
  then completes the above step. -}

addGenreToDB:: [String] -> Int -> Connection -> IO ()
addGenreToDB [] artistPos conn = return ()                                                                                  --Firsly check if there are any genres to process
addGenreToDB (genre:genres) artistPos conn = do
    genreList <- quickQuery' conn "SELECT * FROM Genres WHERE genreName = ?" [toSql genre]                                  --Query the Genre Table to See if the Genre already exists
    if (length genreList) == 0 then do                                                                                      --If it doesn't
        run conn "INSERT INTO Genres (genreName) VALUES (?)" [toSql genre]                                                  --Insert the new genre into the table
        genreList' <- quickQuery' conn "SELECT * FROM Genres WHERE genreName = ?" [toSql genre]                             --Get the Database ID of the new Genre
        let genrePos = (fromSql $ head $ head $ genreList' ::Int)                                                           --Extract ID value from SQLValue list
        run conn "INSERT INTO ArtistGenres (artist,genreID) VALUES (?,?)" [toSql (artistPos::Int), toSql (genrePos::Int)]   --Insert the Artist and genre ID's into the conjoing table
        addGenreToDB genres artistPos conn                                                                                  --Recall the method to save any remaning genres
        return ()
    else do                                                                                                                 --If the Genre does exist
        let genrePos = (fromSql $ head $ head $ genreList ::Int)                                                            --Then Only the second insertion needs to be completed.
        run conn "INSERT INTO ArtistGenres (artist,genreID) VALUES (?,?)" [toSql (artistPos::Int), toSql (genrePos::Int)]
        addGenreToDB genres artistPos conn
        return ()

{-| The addAlbumToDB function takes a list of albums, the name of the artist
    that the albums belong to, and a connection object. It firstly checks
    if any of the albums are duplicates (as there are many in the spotify
    api). For the albums for which this is not the case, it adds their 
    information to the albums table, and their artwork to the images
    table. -}
addAlbumToDB:: [Album] -> String -> Connection -> IO ()
addAlbumToDB [] artist conn = return ()                                                                      --Firstly check if there are any More albums to process
addAlbumToDB (album:albums) artist conn = do
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql (artist)]             --Query the database to get the DB ID for the given artist name 
    let artistId = fromSql $ head $ head $ artistList ::Int                                                  --Extract the ID from the SQLValue list
    let sameAlbums = "SELECT * FROM Albums WHERE LOWER(albumName) = LOWER(?) AND albumArtist = ?"            --Prepare a statement to check if an album by this artist with the same name exists
    albumList <- quickQuery' conn sameAlbums [toSql (albumName album), toSql (artistId::Int)]                --Execute the query
    if(albumList==[]) then do                                                                                --Check the return SQLValue list to see if it contains any data, if it doesn't
      addAlbum album artistId conn                                                                           --Add the album to the database
      addAlbumToDB albums artist conn                                                                        --And recall the function to deal with the rest of the albums
    else addAlbumToDB albums artist conn                                                                     --If an album of that name does exist for the given artist it is ignored

{-AddAlbum is a helper function for addAlbumToDB. If the album is found to
  not be a duplicate in addALbumToDB it is passed here to be inserted into
  the albums table. It also calls the addImagesToDB function to deal with
  the albums artwork-}
addAlbum :: Album -> Int -> Connection -> IO ()
addAlbum album artistId conn = do
    let stmt = "INSERT INTO Albums (albumName,albumSpotifyID,albumArtist) VALUES (?, ?, ?)"                  --Prepare a statment to insert the Album data into the album table
    run conn stmt [toSql (albumName album),                                                                  --Run the statement, extracting information from the Album object and converting to SQLValues
                   toSql (albumID album),
                   toSql ((artistId)::Int)]
    albumList <- quickQuery' conn "SELECT * FROM Albums WHERE albumSpotifyID = ?" [toSql (albumID album)]    --Query the Albums table for the DB ID of the album
    let albumPos = (fromSql $ head $ head $ albumList ::Int)                                                 --Extract the ID from the SQLValue list
    addImagesToDB (albumImages album) albumPos conn                                                          --Add the images for the album 

{-addImagesToDB is another helper function for addAlbumToDB. It takes a 
  a list of images, the DB ID of the related album and a connection 
  object. It then adds all of the images into the images table.-}
addImagesToDB:: [Image] -> Int -> Connection -> IO ()
addImagesToDB [] albumPos conn = return ()                                                                   --Firstly check to make sure that there are images to process
addImagesToDB (image:images) albumPos conn = do
    let stmt = "INSERT INTO Images (imageURL,height,width,albumID) VALUES (?,?,?,?)"                         --Prepare a statement for inserting the Image information into the image table
    run conn stmt [toSql (imageurl image),                                                                   --Run the statement, extracting information from the Image object and converting to SQLValues
                   toSql ((height image)::Int),
                   toSql ((width image)::Int),
                   toSql (albumPos::Int)]
    addImagesToDB images albumPos conn                                                                       --Recall the function to process the remaining images 

{-| The addTracksToDB function takes a list of Tracks a spotiyf album
    ID (String) and a connection object. It first checks that the album
    ID is in the database (due to removing the doubles), and if it is
    adds all of the tracks in the list into the tracks table.-}
addTracksToDB:: [Track] -> String -> Connection -> IO ()
addTracksToDB tracks albumId conn = do
    albumList <- quickQuery' conn "SELECT * FROM Albums WHERE albumSpotifyID = ?" [toSql (albumId)]          --Query the albums table to get the DB ID of the album with the given spotify ID
    if(albumList==[]) then return ()                                                                         --If the album does not exist (due to removing doubles) return
    else do                                                                                                  --Otherwise
        let albumPos = (fromSql $ head $ head $ albumList ::Int)                                             --extract the DB ID from the SQLValue list
        addTracks tracks albumPos conn                                                                       --And add the tracks for the album
        return ()

{-AddTracks is a helper function for addTracksToDB. Once the Track list
  has been found to have an album in the database, it is sent here to 
  be added into the track table.-}
addTracks:: [Track] -> Int -> Connection -> IO ()
addTracks [] albumPos conn = return ()                                                                                              --Firstly check to make sure there are tracks to process
addTracks (track:tracks) albumPos conn = do
    let stmt = "INSERT INTO Tracks (trackName,trackNum,trackSpotifyID,explicit,duration,previewURL,album) VALUES (?,?,?,?,?,?,?)"   --Preparse a statement to insert the data into the Track table
    run conn stmt [toSql ( trackName track),                                                                                        --Run the statement, extracting the information from the Track object and converting to SQL values
                   toSql ((trackNum  track)::Int),
                   toSql ( trackId   track),
                   toSql ((explicit  track)::Bool),
                   toSql ((duration  track)::Int),
                   toSql ( preview   track),
                   toSql (albumPos::Int)]
    addTracks tracks albumPos conn                                                                                                  --Recall the function to process the rest of the albums