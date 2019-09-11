{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (Spec, hspec)

import qualified CheckDatastore as Datastore (spec)
import qualified CheckServer as Server (spec)
import qualified MockData as Mock (spec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    Datastore.spec
    Mock.spec
    Server.spec

