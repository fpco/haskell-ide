{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.ProjectTemplate.RenderSpec (spec) where

import ClassyPrelude
import Test.Hspec
import Text.ProjectTemplate

spec :: Spec
spec = do
    describe "eval" $ do
        it "literals" $ evalBool empty (ExpLiteralBool True) `shouldBe` Success True
    describe "renderFile" $ do
        it "simple file" $ renderFile empty File
            { fileName = ExpLiteralText "foo.txt"
            , fileToGenerate = ExpLiteralBool True
            , fileContents = FileContentsByteString "Hello World"
            } `shouldBe` Success (singleton "foo.txt" (Right "Hello World"))
        it "conditional file, excluded" $ renderFile uv File
            { fileName = ExpLiteralText "foo.txt"
            , fileToGenerate = ExpNot $ ExpEq (ExpFieldText "name") (ExpLiteralText "michael")
            , fileContents = FileContentsByteString "Hello World"
            } `shouldBe` Success empty
        it "conditional file, included" $ renderFile uv File
            { fileName = ExpLiteralText "foo.txt"
            , fileToGenerate = ExpEq (ExpFieldText "name") (ExpLiteralText "michael")
            , fileContents = FileContentsByteString "Hello World"
            } `shouldBe` Success (singleton "foo.txt" (Right "Hello World"))
        it "conditional file, invalid spec" $ renderFile uv File
            { fileName = ExpLiteralText "foo.txt"
            , fileToGenerate = ExpNot $ ExpEq (ExpFieldText "male") (ExpLiteralText "michael")
            , fileContents = FileContentsByteString "Hello World"
            } `shouldBe` Failure (singleton $ ExpectedText "male" (UVBool True))
    describe "renderProject" $ do
        it "works" $ do
            renderProject uv project `shouldBe` Success (singleton "michael-man.txt" $ Right "THIS IS A FILE")
  where
    uv = asHashMap $ fromList
        [ ("name", UVText "michael")
        , ("male", UVBool True)
        ]
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
                    , ExpConditional (ExpNot (ExpFieldBool "male")) $ ExpLiteralText "woman"
                    , ExpLiteralText ".txt"
                    ]
                , fileToGenerate = ExpLiteralBool True
                , fileContents = FileContentsByteString "THIS IS A FILE"
                }
            ]
        }
