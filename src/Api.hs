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

data User = User {
  email: String,
  Tags: List String
} deriving(Show, ToJson)

newtype Status = Status {
  code: Int
} deriving(Show, ToJson)

type UserApi
   = "create" :> ReqBody '[ JSON] User :> Post '[ JSON] [Status]

userApi :: Proxy UserApi
userApi = Proxy
