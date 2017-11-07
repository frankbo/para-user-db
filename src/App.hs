{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module App where

import           Api
import           Control.Monad.IO.Class   (liftIO)
import           Network.Wai.Handler.Warp
import           Prelude
import           Servant
import           System.IO.Error

createUser :: User -> Handler Status
createUser user = do
  _ <- liftIO $ print user
  return (Status 200)

readUser :: UserId -> Handler User
readUser = undefined

deleteUser :: UserId -> Handler Status
deleteUser = undefined

server :: Server UserApi
server = createUser :<|> readUser :<|> deleteUser

startServer :: IO ()
startServer =
  catchIOError
    (do _ <- print "running on 8082"
        run 8082 $ serve userApi server)
    (print . (++) "failed startup with: " . show)
