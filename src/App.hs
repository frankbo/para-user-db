{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           Api
import           Network.Wai.Handler.Warp
import           Prelude                  ()
import           Prelude.Compat
import           Servant
import           System.IO.Error

server :: Server UserApi
server = undefined

startServer :: IO ()
startServer =
  catchIOError
    (do _ <- print "running on 8082"
        run 8082 $ serve userApi server)
    (print . (++) "failed startup with: " . show)
