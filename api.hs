{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), (:<|>)((:<|>)), Get, Post, Capture, JSON, Proxy(..), ServantErr, ServerT, serve )
import Data.Aeson
import Control.Applicative
import GHC.Generics

-- Record store and filtering

data Thing = Thing
    { tid :: Integer
    , key :: String
    , value :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Thing

things :: [Thing]
things = [ Thing 1 "sheep" "thirty"
         , Thing 2 "dragons" "old"
         , Thing 3 "flowers" "pretty"
         ]


-- "fake" error handling for now until I understand the types better
findId :: Integer -> [Thing] -> Thing
findId thId [] = Thing 0 "err" "error"
findId thId (x:y) | (tid x) == thId = x
                  | otherwise = findId thId y


-- API routing and application


type ThingAPI = "things" :> Capture "thingpk" Integer :> Get '[Servant.JSON] Thing
           :<|> "things" :> Get '[Servant.JSON] [Thing]


thingAPI :: ServerT ThingAPI (EitherT ServantErr IO)
thingAPI = thingsDetail :<|> thingsList


thingsDetail :: Integer -> EitherT ServantErr IO Thing
thingsDetail thingpk = return (findId thingpk things)


thingsList :: EitherT ServantErr IO [Thing]
thingsList = return things


-- Run the server

app :: Application
app = serve (Proxy :: Proxy ThingAPI) thingAPI


main :: IO ()
main = run 32323 $ logStdoutDev app
