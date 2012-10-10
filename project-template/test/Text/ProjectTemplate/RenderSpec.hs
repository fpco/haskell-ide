{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.ProjectTemplate.RenderSpec (spec) where

import ClassyPrelude
import Test.Hspec
import Text.ProjectTemplate

spec :: Spec
spec = do
    describe "eval" $ do
        it "literals" $ eval empty (ExpLiteral True) `shouldBe` Success True
    describe "renderFile" $ do
        it "simple file" $ renderFile empty File
            { fileName = singleton $ ContentText $ ExpLiteral "foo.txt"
            , fileToGenerate = ExpLiteral True
            , fileContents = Right "Hello World"
            } `shouldBe` Success (singleton "foo.txt" (Right "Hello World"))
        it "conditional file, excluded" $ renderFile uv File
            { fileName = singleton $ ContentText $ ExpLiteral "foo.txt"
            , fileToGenerate = ExpNe (ExpFieldText "name") (ExpLiteral "michael")
            , fileContents = Right "Hello World"
            } `shouldBe` Success empty
        it "conditional file, included" $ renderFile uv File
            { fileName = singleton $ ContentText $ ExpLiteral "foo.txt"
            , fileToGenerate = ExpEq (ExpFieldText "name") (ExpLiteral "michael")
            , fileContents = Right "Hello World"
            } `shouldBe` Success (singleton "foo.txt" (Right "Hello World"))
        it "conditional file, invalid spec" $ renderFile uv File
            { fileName = singleton $ ContentText $ ExpLiteral "foo.txt"
            , fileToGenerate = ExpNe (ExpFieldText "male") (ExpLiteral "michael")
            , fileContents = Right "Hello World"
            } `shouldBe` Failure (singleton $ ExpectedText "male" (UVBool True))
    describe "renderProject" $ do
        it "works" $ do
            renderProject uv project `shouldBe` Success (singleton "michael-man.txt" $ Right "THIS IS A FILE")
  where
    uv = fromList
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
                { fileName = fromList
                    [ ContentText $ ExpFieldText "name"
                    , ContentText $ ExpLiteral "-"
                    , ContentConditional (ExpFieldBool "male") $ singleton $ ContentText $ ExpLiteral "man"
                    , ContentConditional (ExpEq (ExpFieldBool "male") (ExpLiteral False))
                        $ singleton $ ContentText $ ExpLiteral "woman"
                    , ContentText $ ExpLiteral ".txt"
                    ]
                , fileToGenerate = ExpLiteral True
                , fileContents = Right "THIS IS A FILE"
                }
            ]
        }
