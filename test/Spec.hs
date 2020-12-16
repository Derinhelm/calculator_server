{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main (main) where

import  Lib  (app)
import           Test.Hspec
import           Test.Hspec.Wai
--import           Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /calc/add" $ do
        it "A right addition" $ do
            get "/calc/add/4/5" `shouldRespondWith` "{\"resOperator\":\"add\",\"resArguments\":[4,5],\"resError\":null,\"resResult\":9}"
    describe "GET /calc/sub" $ do
        it "A right subtraction" $ do
            get "/calc/sub/4/5" `shouldRespondWith` "{\"resOperator\":\"sub\",\"resArguments\":[4,5],\"resError\":null,\"resResult\":-1}"
    describe "GET /calc/mul" $ do
        it "A right multiplication" $ do
            get "/calc/mul/4/5" `shouldRespondWith` "{\"resOperator\":\"mul\",\"resArguments\":[4,5],\"resError\":null,\"resResult\":20}"
    describe "GET /calc/div" $
        it "A right division" $
            get "/calc/div/4/5" `shouldRespondWith` "{\"resOperator\":\"div\",\"resArguments\":[4,5],\"resError\":null,\"resResult\":0.8}"
    describe "GET /calc/sqrt" $
        it "A right root extraction" $
            get "/calc/sqrt/0.25" `shouldRespondWith` "{\"resOperator\":\"sqrt\",\"resArguments\":[0.25],\"resError\":null,\"resResult\":0.5}"
    describe "GET /calc/pow" $
        it "A right exponentiation" $
            get "/calc/pow/5/2" `shouldRespondWith` "{\"resOperator\":\"pow\",\"resArguments\":[5,2],\"resError\":null,\"resResult\":25}"
    describe "GET /calc/div" $
        it "A wrong division" $
            get "/calc/div/4/0" `shouldRespondWith` "{\"resOperator\":\"div\",\"resArguments\":[4,0],\"resError\":\"Division by zero\",\"resResult\":null}"
    describe "GET /calc/sqrt" $
        it "A wrong root extraction" $
            get "/calc/sqrt/-5" `shouldRespondWith` "{\"resOperator\":\"sqrt\",\"resArguments\":[-5],\"resError\":\"Extracting the root from the negative\",\"resResult\":null}"
    describe "GET /calc/pow" $
        it "A wrong exponentiation" $
            get "/calc/pow/0/-1" `shouldRespondWith` "{\"resOperator\":\"pow\",\"resArguments\":[0,-1],\"resError\":\"Raising zero to a negative power\",\"resResult\":null}"
