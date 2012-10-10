{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.ProjectTemplate.JSONSpec (spec) where

import ClassyPrelude
import Test.Hspec
import Text.ProjectTemplate
import Data.Aeson

spec :: Spec
spec = do
    describe "JSON" $ do
        it "decode . encode == Just" $ fromJSON (toJSON project) `shouldBe` Data.Aeson.Success project
  where
    project = Project
        { projectName = "dummy"
        , projectDescription = "dummy"
        , projectFields = fromList
            [ Field "name" "Person's name" (FTText Nothing)
            , Field "male" "Is person male?" FTBool
            ]
        , projectFiles = fromList
            [ File
                { fileName = ExpConcat $ fromList
                    [ ExpFieldText "name"
                    , ExpLiteralText "-"
                    , ExpConditional (ExpFieldBool "male") $ ExpLiteralText "man"
                    , ExpConditional (ExpNot $ ExpFieldBool "male") $ ExpLiteralText "woman"
                    , ExpLiteralText ".txt"
                    ]
                , fileToGenerate = ExpLiteralBool True
                , fileContents = Right "THIS IS A FILE"
                }
            ]
        }
