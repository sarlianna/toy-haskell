{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Either (EitherT)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
    ( (:>), Get, Capture, JSON, Proxy(..), ServantErr, ServerT, serve )


type EchoAPI = "echo" :> Capture "echoIn" String :> Get '[Servant.JSON] String


echoAPI :: ServerT EchoAPI (EitherT ServantErr IO)
echoAPI = echo


echo :: String -> EitherT ServantErr IO String
echo echoIn = return echoIn


app :: Application
app = serve (Proxy :: Proxy EchoAPI) echoAPI


main :: IO ()
main = run 32323 $ logStdoutDev app
