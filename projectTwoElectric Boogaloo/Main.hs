{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy.Char8 as BU
import Data.Text (unpack)
import Data.Monoid
import GenerateHTML
import Data.String.Conversions 
import Data.Maybe

{- This project runs a warp server which responds to requests for artist information
   The full details for this project can be found in the pdf located in the project
   Description folder-}

{-| The main method runs the server and sends any request to the app function below -}
main = do
    let port = 80                                                                               --The port to listen on is selected (80 as this is the default HTTP port)
    putStrLn $ "Listening on port " ++ show port                                                --Print to the terminal to denote the server is listening
    run port app                                                                                --Run the server on the set port using the below warp application

{-| The app function specifies how requests to the server should be handled,
    this includes sending the user to the index page if an artist is yet to be requested
    and building/returning the artist page once a query is sent -}
app :: Application
app req respond = do
    let query = pathInfo req                                                                    --Extract Url Path Name
    if (length query < 1) then do                                                               --If there is not a URL path name (no artist requested)
        homepage <- readFile "HTML/index.html"                                                  --Read in the index (homepage)
        respond $ responseLBS status200 [("Content-Type", "text/HTML")] (BU.pack homepage)      --And return it with to the user
    else do                                                                                     --If there is a URL Path Name
        let artist = head $ map unpack $ query                                                  --Extract the name of the artist 
        artistpage <- (createPage (artist));                                                    --Create the page for the artist with the GenerateHTML createPage function
        respond $ responseLBS status200 [("Content-Type", "text/HTML")] (BU.pack artistpage)    --And return the page to the user
 
