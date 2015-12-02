module SpotifyDatabase (addArtistToDB, addAlbumToDB, addTracksToDB, createDatabase) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import SpotifyDataTypes

addArtistToDB:: FullArtist -> IO ()
addArtistToDB artist = do
    conn <- connectMySQL defaultMySQLConnectInfo {
                  mysqlHost = "localhost",
                  mysqlDatabase = "spotify",
                  mysqlUser = "root",
                  mysqlPassword = "1234",
                  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" }
    let stmt = "INSERT INTO Artists (artistName,artistSpotifyID,artistFollowers,artistPopularity) VALUES (?, ?, ?, ?)"
    run conn stmt [toSql (name artist),
                   toSql (identifier artist),
                   toSql ((followers artist)::Int),
                   toSql ((popularity artist)::Double)]
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql (name artist)]
    addGenreToDB (genres artist) (fromSql $ head $ head $ artistList ::Int) conn
    commit conn 
    disconnect conn


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

addAlbumToDB:: [Album] -> String -> IO ()
addAlbumToDB albums artist = do
    conn <- connectMySQL defaultMySQLConnectInfo {
                  mysqlHost = "localhost",
                  mysqlDatabase = "spotify",
                  mysqlUser = "root",
                  mysqlPassword = "1234",
                  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" }
    addAlbums albums artist conn
    commit conn 
    disconnect conn 
    return ()

addAlbums:: [Album] -> String -> Connection -> IO ()
addAlbums [] artist conn = return ()
addAlbums (album:albums) artist conn = do
    artistList <- quickQuery' conn "SELECT * FROM Artists WHERE artistName = ?" [toSql (artist)]
    let artistId = fromSql $ head $ head $ artistList ::Int
    let stmt = "INSERT INTO Albums (albumName,albumSpotifyID,albumArtist) VALUES (?, ?, ?)"
    run conn stmt [toSql (albumName album),
                   toSql (albumID album),
                   toSql ((artistId)::Int)]
    albumList <- quickQuery' conn "SELECT * FROM Albums WHERE albumSpotifyID = ?" [toSql (albumID album)]
    let albumPos = (fromSql $ head $ head $ albumList ::Int)
    addImagesToDB (albumImages album) albumPos conn
    addAlbums albums artist conn
    return ()

addImagesToDB:: [Image] -> Int -> Connection -> IO ()
addImagesToDB [] albumPos conn = return ()
addImagesToDB (image:images) albumPos conn = do
    let stmt = "INSERT INTO Images (imageURL,height,width,albumID) VALUES (?,?,?,?)"
    run conn stmt [toSql (imageurl image),
                   toSql ((height image)::Int),
                   toSql ((width image)::Int),
                   toSql (albumPos::Int)]
    addImagesToDB images albumPos conn

addTracksToDB:: [Track] -> String -> IO ()
addTracksToDB tracks albumId = do
    conn <- connectMySQL defaultMySQLConnectInfo {
                  mysqlHost = "localhost",
                  mysqlDatabase = "spotify",
                  mysqlUser = "root",
                  mysqlPassword = "1234",
                  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" }
    albumList <- quickQuery' conn "SELECT * FROM Albums WHERE albumSpotifyID = ?" [toSql (albumId)]
    let albumPos = (fromSql $ head $ head $ albumList ::Int)
    addTracks tracks albumPos conn
    commit conn 
    disconnect conn 
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



createDatabase = do
    conn <- connectMySQL defaultMySQLConnectInfo {
                mysqlHost = "localhost",
                mysqlDatabase = "spotify",
                mysqlUser = "root",
                mysqlPassword = "1234",
                mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" }
    createArtist conn
    createAlbums conn
    createImages conn
    createGenres conn
    createTracks conn
    createArtGen conn
    commit conn
    disconnect conn

createArtist conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `Artists` (",
                                       "`idArtists` INT NOT NULL AUTO_INCREMENT,",
                                       "`artistName` VARCHAR(100) NOT NULL,",
                                       "`artistSpotifyID` VARCHAR(45) NOT NULL,",
                                       "`artistFollowers` INT NOT NULL,",
                                       "`artistPopularity` DECIMAL(2) NOT NULL,",
                                       "PRIMARY KEY (`idArtists`))"]) []

createAlbums conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `Albums` (",
                                       "`idAlbums` INT NOT NULL AUTO_INCREMENT,",
                                       "`albumName` VARCHAR(100) NOT NULL,",
                                       "`albumSpotifyID` VARCHAR(45) NOT NULL,",
                                       "`albumArtist` INT NOT NULL,",
                                       "PRIMARY KEY (`idAlbums`),",
                                       "INDEX `albumArtist_idx` (`albumArtist` ASC),",
                                       "CONSTRAINT `albumArtist`",
                                       "FOREIGN KEY (`albumArtist`)",
                                       "REFERENCES `Artists` (`idArtists`)",
                                       "ON DELETE CASCADE",
                                       "ON UPDATE NO ACTION)"]) []

createImages conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `Images`(",
                                       "`idImages` INT NOT NULL AUTO_INCREMENT,",
                                       "`imageURL` VARCHAR(200) NOT NULL,",
                                       "`height` INT NOT NULL,",
                                       "`width` INT NOT NULL,",
                                       "`albumID` INT NOT NULL,",
                                       "PRIMARY KEY (`idImages`),",
                                       "INDEX `albumID_idx` (`albumID` ASC),",
                                       "CONSTRAINT `albumID`",
                                       "FOREIGN KEY (`albumID`)",
                                       "REFERENCES `Albums` (`idAlbums`)",
                                       "ON DELETE CASCADE",
                                       "ON UPDATE NO ACTION)"]) []

createGenres conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `Genres` (",
                                       "`idGenres` INT NOT NULL AUTO_INCREMENT,",
                                       "`genreName` VARCHAR(45) NOT NULL,",
                                       "PRIMARY KEY (`idGenres`))"]) []

createTracks conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `Tracks` (",
                                       "`idTracks` INT NOT NULL AUTO_INCREMENT,",
                                       "`trackName` VARCHAR(100) NOT NULL,",
                                       "`trackNum` INT NOT NULL,",
                                       "`trackSpotifyID` VARCHAR(45) NOT NULL,",
                                       "`explicit` TINYINT(1) NOT NULL,",
                                       "`duration` INT NOT NULL,",
                                       "`previewURL` VARCHAR(100) NOT NULL,",
                                       "`album` INT NOT NULL,",
                                       "PRIMARY KEY (`idTracks`),",
                                       "INDEX `album_idx` (`album` ASC),",
                                       "CONSTRAINT `album`",
                                       "FOREIGN KEY (`album`)",
                                       "REFERENCES `Albums` (`idAlbums`)",
                                       "ON DELETE CASCADE",
                                       "ON UPDATE NO ACTION)"]) []

createArtGen conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `ArtistGenres` (",
                                       "`idArtistGenres` INT NOT NULL AUTO_INCREMENT,",
                                       "`artist` INT NOT NULL,",
                                       "`genreID` INT NOT NULL,",
                                       "PRIMARY KEY (`idArtistGenres`),",
                                       "INDEX `artist_idx` (`artist` ASC),",
                                       "INDEX `gereID_idx` (`genreID` ASC),",
                                       "CONSTRAINT `artist`",
                                       "FOREIGN KEY (`artist`)",
                                       "REFERENCES `Artists` (`idArtists`)",
                                       "ON DELETE CASCADE",
                                       "ON UPDATE NO ACTION,",
                                       "CONSTRAINT `gereID`",
                                       "FOREIGN KEY (`genreID`)",
                                       "REFERENCES `Genres` (`idGenres`)",
                                       "ON DELETE CASCADE",
                                       "ON UPDATE NO ACTION)"]) []