{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import           Data.Aeson.Types
import           GHC.Generics
import           Prelude          ()
import           Prelude.Compat
import           Servant

data User = User
  { email :: String
  , tags  :: [String]
  } deriving (Show, Generic)

instance ToJSON User

instance FromJSON User

newtype Status = Status
  { code :: Int
  } deriving (Generic)

instance ToJSON Status

type UserApi
   = "create" :> ReqBody '[ JSON] User :> Post '[ JSON] Status :<|> "read" :> ReqBody '[ JSON] User :> Post '[ JSON] User :<|> "delete" :> ReqBody '[ JSON] User :> Post '[ JSON] Status

userApi :: Proxy UserApi
userApi = Proxy
