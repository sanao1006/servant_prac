{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}


module Lib
    ( runServant
    ) where

import Servant
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

isaac :: User
isaac = User "Isaac Newton" 372 "isac@newton.co.uk"(fromGregorian 1683 3 1)
albert :: User
albert = User "Albert Einstein" 136 "ae@mc2.org"(fromGregorian 1905 12 1)

sanao :: User
sanao = User "Sanao" 17 "sanasana@sana" (fromGregorian 2017 1 1)

users2 :: [User]
users2 = [isaac, albert,sanao]


type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User
           :<|> "sanao" :> Get '[JSON] User

server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac
     :<|> return sanao

userAPI :: Proxy UserAPI2
userAPI = Proxy

app2 :: Application
app2 = serve userAPI server2

runServant :: IO ()
runServant = run 8081 app2

