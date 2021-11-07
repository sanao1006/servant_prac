{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}


module Lib
    ( runServant
    ) where

import Servant(serve, Proxy(..), Server, JSON, Get,(:>))
import Data.Aeson(ToJSON)
import Data.Time.Calendar
import GHC.Generics(Generic)
import Network.Wai(Application)
import Network.Wai.Handler.Warp(run)

data User = User
 {
     name :: String,
     age :: Int,
     email :: String,
     registration_date :: Day
 } deriving(Eq,Show, Generic)

instance ToJSON User

users1 :: [User]
users1 =
    [
        User "Isac Newton" 372 "isac@newton.co.uk" (fromGregorian 1683 3 1),
        User "Albert Einstein" 136 "ae@mc2.org"         (fromGregorian 1905 12 1)
    ]

type UserAPI1 = "users" :> Get '[JSON] [User]

server1 :: Server UserAPI1
server1 = return users1

userAPI :: Proxy UserAPI1
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

runServant :: IO ()
runServant = run 8081 app1

