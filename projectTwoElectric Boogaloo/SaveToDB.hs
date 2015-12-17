module SaveToDB (checkArtistInDB,addArtistToDB, addAlbumToDB, addTracksToDB) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import MySqlConnect
import SpotifyDataTypes


checkArtistInDB:: String -> Connection -> IO Bool
checkArtistInDB stringID conn = do
    genreList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql stringID]
    if (length genreList) == 0 then return False
    else return True
    

addArtistToDB:: FullArtist -> Connection -> IO ()
addArtistToDB artist conn = do
    let stmt = "INSERT INTO Artists (artistName,artistSpotifyID,artistFollowers,artistPopularity) VALUES (?, ?, ?, ?)"
    run conn stmt [toSql (name artist),
                   toSql (identifier artist),
                   toSql ((followers artist)::Int),
                   toSql ((popularity artist)::Double)]
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql (name artist)]
    addGenreToDB (genres artist) (fromSql $ head $ head $ artistList ::Int) conn


addGenreToDB:: [String] -> Int -> Connection -> IO ()
addGenreToDB [] artistPos conn = return ()
addGenreToDB (genre:genres) artistPos conn = do
    genreList <- quickQuery' conn "SELECT * FROM Genres WHERE genreName = ?" [toSql genre]
    if (length genreList) == 0 then do
        run conn "INSERT INTO Genres (genreName) VALUES (?)" [toSql genre]
        genreList' <- quickQuery' conn "SELECT * FROM Genres WHERE genreName = ?" [toSql genre]
        let genrePos = (fromSql $ head $ head $ genreList' ::Int)
        run conn "INSERT INTO ArtistGenres (artist,genreID) VALUES (?,?)" [toSql (artistPos::Int), toSql (genrePos::Int)]
        addGenreToDB genres artistPos conn
        return ()
    else do
        let genrePos = (fromSql $ head $ head $ genreList ::Int)
        run conn "INSERT INTO ArtistGenres (artist,genreID) VALUES (?,?)" [toSql (artistPos::Int), toSql (genrePos::Int)]
        addGenreToDB genres artistPos conn
        return ()

addAlbumToDB:: [Album] -> String -> Connection -> IO ()
addAlbumToDB [] artist conn = return ()
addAlbumToDB (album:albums) artist conn = do
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql (artist)]
    let artistId = fromSql $ head $ head $ artistList ::Int
    let sameAlbums = "SELECT * FROM Albums WHERE LOWER(albumName) = LOWER(?) AND albumArtist = ?" 
    albumList <- quickQuery' conn sameAlbums [toSql (albumName album), toSql (artistId::Int)]
    if(albumList==[]) then do
      addAlbum album artistId conn
      addAlbumToDB albums artist conn
    else addAlbumToDB albums artist conn

addAlbum :: Album -> Int -> Connection -> IO ()
addAlbum album artistId conn = do
    let stmt = "INSERT INTO Albums (albumName,albumSpotifyID,albumArtist) VALUES (?, ?, ?)"
    run conn stmt [toSql (albumName album),
                   toSql (albumID album),
                   toSql ((artistId)::Int)]
    albumList <- quickQuery' conn "SELECT * FROM Albums WHERE albumSpotifyID = ?" [toSql (albumID album)]
    let albumPos = (fromSql $ head $ head $ albumList ::Int)
    addImagesToDB (albumImages album) albumPos conn

addImagesToDB:: [Image] -> Int -> Connection -> IO ()
addImagesToDB [] albumPos conn = return ()
addImagesToDB (image:images) albumPos conn = do
    let stmt = "INSERT INTO Images (imageURL,height,width,albumID) VALUES (?,?,?,?)"
    run conn stmt [toSql (imageurl image),
                   toSql ((height image)::Int),
                   toSql ((width image)::Int),
                   toSql (albumPos::Int)]
    addImagesToDB images albumPos conn

addTracksToDB:: [Track] -> String -> Connection -> IO ()
addTracksToDB tracks albumId conn = do
    albumList <- quickQuery' conn "SELECT * FROM Albums WHERE albumSpotifyID = ?" [toSql (albumId)]
    if(albumList==[]) then return ()
    else do
        let albumPos = (fromSql $ head $ head $ albumList ::Int)
        addTracks tracks albumPos conn 
        return ()

addTracks:: [Track] -> Int -> Connection -> IO ()
addTracks [] albumPos conn = return ()
addTracks (track:tracks) albumPos conn = do
    let stmt = "INSERT INTO Tracks (trackName,trackNum,trackSpotifyID,explicit,duration,previewURL,album) VALUES (?,?,?,?,?,?,?)"
    run conn stmt [toSql ( trackName track),
                   toSql ((trackNum  track)::Int),
                   toSql ( trackId   track),
                   toSql ((explicit  track)::Bool),
                   toSql ((duration  track)::Int),
                   toSql ( preview   track),
                   toSql (albumPos::Int)]
    addTracks tracks albumPos conn