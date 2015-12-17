module CreateDB (createDatabase) where

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import SpotifyDataTypes
import MySqlConnect

{-| The createDatabase function is used to initialise the database for the server/project to run.
    It assumes that there is already a database called Spotify as this is required for the 
    MySql connection object.

    The function itself runs six individual functions, one for each table required in the database.
    Inside the file for this module you will see that each of these functions corresponds to running
    a singular SQL command to build the table, and the details for these tables can either be found
    there or in the project pdf. -}
createDatabase :: IO ()
createDatabase = do
    conn <- getConnection
    createArtist conn
    createAlbums conn
    createImages conn
    createGenres conn
    createTracks conn
    createArtGen conn
    closeConnection conn

createArtist:: Connection -> IO Integer
createArtist conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `Artists` (",
                                       "`idArtists` INT NOT NULL AUTO_INCREMENT,",
                                       "`artistName` VARCHAR(100) NOT NULL,",
                                       "`artistSpotifyID` VARCHAR(45) NOT NULL,",
                                       "`artistFollowers` INT NOT NULL,",
                                       "`artistPopularity` DECIMAL(2) NOT NULL,",
                                       "PRIMARY KEY (`idArtists`))"]) []

createAlbums:: Connection -> IO Integer
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

createImages:: Connection -> IO Integer
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

createGenres:: Connection -> IO Integer
createGenres conn = run conn (unlines ["CREATE TABLE IF NOT EXISTS `Genres` (",
                                       "`idGenres` INT NOT NULL AUTO_INCREMENT,",
                                       "`genreName` VARCHAR(45) NOT NULL,",
                                       "PRIMARY KEY (`idGenres`))"]) []

createTracks:: Connection -> IO Integer
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

createArtGen:: Connection -> IO Integer
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