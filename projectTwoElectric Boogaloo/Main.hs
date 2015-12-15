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
main = do
    let port = 80
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req respond = do
    let query = pathInfo req
    if (length query < 1) then do
        homepage <- readFile "HTML/index.html"
        respond $ responseLBS status200 [("Content-Type", "text/HTML")] (BU.pack homepage)
    else do
        let artist = head $ map unpack $ query
        artistpage <- (createPage (artist));
        respond $ responseLBS status200 [("Content-Type", "text/HTML")] (BU.pack artistpage)
 
