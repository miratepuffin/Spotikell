{-# LANGUAGE OverloadedStrings #-} --usual string syntax can be used for ByteString, Text, and other variations of string like types

module SpotifyDataTypes where
import Data.Aeson.Types
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text

data Info = Info {
  artists     :: [BasicArtist]
} deriving Show

data BasicArtist = BasicArtist {
  href'       :: String,
  name'       :: String,
  popularity' :: String 
} deriving Show

data FullArtist = FullArtist {
  followers   ::  Int,
  genres      ::  [Text],
  identifier  ::  Text,
  name        ::  Text,
  popularity  ::  Int
} deriving Show

data Albums = Albums {
  albums      :: [Album]
} deriving Show

data Album = Album {
  albumID     :: String,
  albumImages :: [Image],
  albumName   :: String
} deriving Show

data Image = Image {
  height      :: Int,
  imageurl    :: String,
  width       :: Int
} deriving Show

data Tracks = Tracks {
  tracks      :: [Track]
} deriving Show

data Track = Track  {
  duration    :: Int,
  explicit    :: Bool,
  trackId     :: String,
  trackName   :: String,
  preview     :: String,
  trackNum    :: Int
} deriving Show

instance FromJSON Info where
    parseJSON (Object o) = do
      artists <- parseJSON =<< (o .: "artists")
      return $ Info artists
    parseJSON _ = mzero


instance FromJSON BasicArtist where
    parseJSON (Object o) = BasicArtist <$> 
        (o .: "href")
		    <*> (o .: "name")
		    <*> (o .: "popularity")

instance FromJSON FullArtist where
    parseJSON (Object o) = do
       followerBlock <- o .: "followers" 
       followerCount <- followerBlock .: "total"
       genres        <- o .: "genres"
       identifier    <- o .: "id"
       name          <- o .: "name"
       popularity    <- o .: "popularity"
       return $ FullArtist followerCount genres identifier name popularity
    parseJSON _ = mzero

instance FromJSON Albums where
    parseJSON (Object o) = do
      albums <- parseJSON =<< (o .: "items")
      return $ Albums albums
    parseJSON _ = mzero

instance FromJSON Album where
    parseJSON (Object o) = do
       albumId       <- o .: "id"
       images        <- parseJSON =<< (o .: "images")
       albumName     <- o .: "name"
       return $ Album albumId images albumName
    parseJSON _ = mzero

instance FromJSON Image where
    parseJSON (Object o) = do
       height        <- o .: "height"
       url           <- o .: "url"
       width         <- o .: "width"
       return $ Image height url width
    parseJSON _ = mzero

instance FromJSON Tracks where
    parseJSON (Object o) = do
      tracks <- parseJSON =<< (o .: "items")
      return $ Tracks tracks
    parseJSON _ = mzero

instance FromJSON Track where
    parseJSON (Object o) = do
      duration       <- o .: "duration_ms"
      explicit       <- o .: "explicit"
      trackId        <- o .: "id"
      trackName      <- o .: "name"
      preview        <- o .: "preview_url"
      trackNum       <- o .: "track_number"
      return $ Track duration explicit trackId trackName preview trackNum
