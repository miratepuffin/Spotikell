{-# LANGUAGE OverloadedStrings #-}
 
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import qualified Data.ByteString.Lazy.Char8 as BU
import Data.Text (unpack)
import Data.Monoid
import GenerateHTML
import Data.String.Conversions 
main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req respond = do
    html <- createPage (head $ map unpack $ pathInfo req) 
    respond $ responseLBS status200 [("Content-Type", "text/HTML")] (BU.pack html)
 
