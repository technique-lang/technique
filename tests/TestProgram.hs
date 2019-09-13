{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (Spec, hspec, it, describe, shouldBe)

-- import qualified CheckDatastore as Datastore (spec)
-- import qualified CheckServer as Server (spec)
-- import qualified MockData as Mock (spec)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Things" $
        it "Does stuff" $ do
            True `shouldBe` True

--  Datastore.spec
--  Mock.spec
--  Server.spec
