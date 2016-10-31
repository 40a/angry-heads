{-# LANGUAGE OverloadedStrings #-}

module HeadHunterSpec where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as BS
import Test.Hspec

import Network.HeadHunter.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "hh/resumes" $
    it "decoder parses an example response" $ do
        res <- BS.readFile "test/hh_responses/resumes.json"
        decode res `shouldBe`
            (Just $
             Resumes
                 [ Resume
                       [ Company Nothing "Рога и копыта" "http://example.com/"
                       , Company (Just "1455") "HeadHunter" "http://hh.ru"
                       ]
                 ])
