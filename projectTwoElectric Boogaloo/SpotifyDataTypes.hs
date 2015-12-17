{-# LANGUAGE OverloadedStrings #-} --usual string syntax can be used for ByteString, Text, and other variations of string like types

module SpotifyDataTypes where
import Data.Aeson.Types
import Data.Aeson
import Control.Applicative
import Control.Monad
import Data.Text

{-| This is the type used to store all the artists returned from a search -}
data Info = Info {
  artists     :: [BasicArtist]
} deriving Show

{-| The Basic Artist is the information for artists retured from a search
    query. This is then used to find the full artist object -}
data BasicArtist = BasicArtist {
  href'       :: String,
  name'       :: String,
  popularity' :: String 
} deriving Show

{-| The full artist contains all the information about the artist that is
    returned from a query to the Spotify Artist Rest API -}
data FullArtist = FullArtist {
  followers   ::  Int,
  genres      ::  [String],
  identifier  ::  String,
  name        ::  String,
  popularity  ::  Double
} deriving Show

{-| The Albums type stores a list of all albums related to an artist -}
data Albums = Albums {
  albums      :: [Album]
} deriving Show

{-| The Album type stores information on Artist albums, most impotantly
    their ID used to identify the tracks it contains  -}
data Album = Album {
  albumID     :: String,
  albumImages :: [Image],
  albumName   :: String
} deriving Show

{-| The Image type contains the link and dimentions of album artwork -}
data Image = Image {
  height      :: Int,
  imageurl    :: String,
  width       :: Int
} deriving Show

{-| The Tracks type contains a list of all tracks from an Album -}
data Tracks = Tracks {
  tracks      :: [Track]
} deriving Show

{-| The Track type contains metadata about songs available on spotify.
    They also contain the preview  URL used in the project to listen to
    samples of the ALbums-}
data Track = Track  {
  duration    :: Int,
  explicit    :: Bool,
  trackId     :: String,
  trackName   :: String,
  preview     :: String,
  trackNum    :: Int
} deriving Show

{- The Aeson Library requires you to specify that your types
  are instances of FromJSON so that the incoming data may be parsed 
  into them. As such all of the above types have been made instances  -}

instance FromJSON Info where
    parseJSON (Object o) = do
      artists <- parseJSON =<< (o .: "artists")                                    -- Extract the artist list from the JSON
      return $ Info artists                                                        -- Return it in the form of an Info type
    parseJSON _ = mzero


instance FromJSON BasicArtist where
    parseJSON (Object o) = BasicArtist <$>                                          --Return a BasicArtist Type consisting of:
        (o .: "href")                                                               --The href (spotify address) for the artist
		    <*> (o .: "name")                                                           --The name of the artist
		    <*> (o .: "popularity")                                                     --The spotify popularity of the artist

instance FromJSON FullArtist where
    parseJSON (Object o) = do
       followerBlock <- o .: "followers"                                            --Extract the follows object the JSON
       followerCount <- followerBlock .: "total"                                    --Extract the follower count from the follower object
       genres        <- o .: "genres"                                               --Extract the Genre of the artist
       identifier    <- o .: "id"                                                   --Extract the spotify Artist ID (similar to href)
       name          <- o .: "name"                                                 --Extract the artist name 
       popularity    <- o .: "popularity"                                           --Extract the popularity of the artist
       return $ FullArtist followerCount genres identifier name popularity          --Return all this information as in a Full Object Type
    parseJSON _ = mzero                                                             --(as can be seen some overlap with basic, but easier than trying to merge the objects)

instance FromJSON Albums where
    parseJSON (Object o) = do
      albums <- parseJSON =<< (o .: "items")                                        --Parse the 'items' object containing all the album objects
      return $ Albums albums                                                        --When this is returned as an Albums type the Album FromJson method will be automatically
    parseJSON _ = mzero                                                             --called on each of the albums and then these will be stored in the array

instance FromJSON Album where
    parseJSON (Object o) = do
       albumId       <- o .: "id"                                                   --Extract the album ID (similar to the artist href)
       images        <- parseJSON =<< (o .: "images")                               --Parse the Image object containing all artwork (inner objects auto parsed as with albums)
       albumName     <- o .: "name"                                                 --Extract the name of the album
       return $ Album albumId images albumName                                      --Return all information as Album object
    parseJSON _ = mzero

instance FromJSON Image where
    parseJSON (Object o) = do
       height        <- o .: "height"                                               --Extract Height of artwork
       url           <- o .: "url"                                                  --Extract the location of the image online
       width         <- o .: "width"                                                --Extract the Width of the image
       return $ Image height url width                                              --Returned all this information in an Image object
    parseJSON _ = mzero

instance FromJSON Tracks where
    parseJSON (Object o) = do
      tracks <- parseJSON =<< (o .: "items")                                        --As with Albums Tracks just consists of an 'items' object containing the Track objects 
      return $ Tracks tracks                                                        --When this is returned as a Tracks object all of the individual Tracks are parsed
    parseJSON _ = mzero

instance FromJSON Track where
    parseJSON (Object o) = do
      duration       <- o .: "duration_ms"                                          --Extract the length of the song
      explicit       <- o .: "explicit"                                             --Extract weather of not the song has been given an explicit warning
      trackId        <- o .: "id"                                                   --Extract the spotify track ID (similar to album/artist ID)
      trackName      <- o .: "name"                                                 --Extract the name of the track
      preview        <- o .: "preview_url"                                          --Extract the URL where a preview of the song can be found
      trackNum       <- o .: "track_number"                                         --Extract the songs position in its album
      return $ Track duration explicit trackId trackName preview trackNum
