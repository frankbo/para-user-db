{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module App where

import           Api
import           Control.Monad.IO.Class      (liftIO)
import           Database.MongoDB            (PortID (PortNumber), connect, auth,
                                              host, access, master, rest, find, select)
import           Database.MongoDB.Connection (Host(..))
import           Network.Wai.Handler.Warp
import           Prelude
import           Servant
import           System.IO.Error

createUser :: User -> Handler Status
createUser user = do
  _ <- liftIO $ print user
  return (Status 200)

readUser :: UserId -> Handler User
readUser uId = do
  _ <- liftIO $ print uId
  return (User (userId uId) ["first tag"])

deleteUser :: UserId -> Handler Status
deleteUser userId = do
  _ <- liftIO $ print userId
  return (Status 200)

server :: Server UserApi
server = createUser :<|> readUser :<|> deleteUser

startServer :: IO ()
startServer = do
  pipe <- connect (Host "" (PortNumber 31617))
  True <- auth "" ""
  e <- access pipe master "tags" (rest =<< find (select [] "email"))
  _ <- print e
  catchIOError
    (do _ <- print "running on 8082"
        run 8082 $ serve userApi server)
    (print . (++) "failed startup with: " . show)


