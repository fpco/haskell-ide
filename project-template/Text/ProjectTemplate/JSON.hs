{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.ProjectTemplate.JSON () where

import ClassyPrelude
import Text.ProjectTemplate.Types
import Data.Aeson
import Control.Monad (mzero)
import qualified Data.ByteString.Base64 as B64

instance ToJSON Project where
    toJSON Project {..} = object
        [ "name" .= projectName
        , "description" .= projectDescription
        , "user-fields" .= projectFields
        , "files" .= projectFiles
        , "format-version" .= (1 :: Int)
        ]
instance FromJSON Project where
    parseJSON (Object o) = Project
        <$> o .: "name"
        <*> o .: "description"
        <*> o .: "user-fields"
        <*> o .: "files"
    parseJSON _ = mzero

instance ToJSON Field where
    toJSON Field {..} = object
        [ "name" .= fieldName
        , "description" .= fieldDescription
        , "type" .= fieldType
        ]
instance FromJSON Field where
    parseJSON (Object o) = Field
        <$> o .: "name"
        <*> o .: "description"
        <*> o .: "type"
    parseJSON _ = mzero

instance ToJSON FieldType where
    toJSON (FTText Nothing) = "text"
    toJSON (FTText (Just regex)) = String $ "text:" ++ regex
    toJSON FTBool = "bool"
    toJSON (FTEnum options) = toJSON options
instance FromJSON FieldType where
    parseJSON (String "text") = pure $ FTText Nothing
    parseJSON (String t) | Just regex <- stripPrefix "text:" t = pure $ FTText $ Just regex
    parseJSON (String "bool") = pure FTBool
    parseJSON v = FTEnum <$> parseJSON v

instance ToJSON EnumOption where
    toJSON EnumOption {..} = object
        [ "value" .= eoValue
        , "display" .= eoDisplay
        ]
instance FromJSON EnumOption where
    parseJSON (Object o) = EnumOption
        <$> o .: "value"
        <*> o .: "display"
    parseJSON _ = mzero

instance ToJSON File where
    toJSON File {..} = object
        [ "name" .= fileName
        , "togenerate" .= fileToGenerate
        , "contents" .= fileContents
        ]
instance FromJSON File where
    parseJSON (Object o) = File
        <$> o .: "name"
        <*> o .: "togenerate"
        <*> o .: "contents"
    parseJSON _ = mzero

instance ToJSON FileContents where
    toJSON (FileContentsText t) = object
        [ "type" .= asText "text"
        , "value" .= t
        ]
    toJSON (FileContentsByteString bs) = object
        [ "type" .= asText "bytestring"
        , "value" .= B64.encode bs
        ]
instance FromJSON FileContents where
    parseJSON (Object o) = do
        t <- o .: "type"
        v <- o .: "value"
        case asText t of
            "text" -> FileContentsText <$> parseJSON v
            "bytestring" -> parseJSON v >>= either fail (return . FileContentsByteString) . B64.decode
            _ -> fail "Unknown type"
    parseJSON _ = fail "Expected an object"

instance ToJSON BoolExp where
    toJSON (ExpAnd x y) = object
        [ "name" .= String "and"
        , "and1" .= x
        , "and2" .= y
        ]
    toJSON (ExpOr x y) = object
        [ "name" .= String "or"
        , "or1" .= x
        , "or2" .= y
        ]
    toJSON (ExpEq x y) = object
        [ "name" .= String "equal"
        , "equal1" .= x
        , "equal2" .= y
        ]
    toJSON (ExpNot x) = object
        [ "name" .= String "not"
        , "bool" .= x
        ]
    toJSON (ExpFieldBool x) = object
        [ "name" .= String "field"
        , "field-name" .= x
        ]
    toJSON (ExpLiteralBool x) = object
        [ "name" .= String "literal"
        , "value" .= x
        ]
instance FromJSON BoolExp where
    parseJSON (Object o) = do
        name <- o .: "name"
        case name :: Text of
            "and" -> ExpAnd <$> o .: "and1" <*> o .: "and2"
            "or" -> ExpOr <$> o .: "or1" <*> o .: "or2"
            "equal" -> ExpEq <$> o .: "equal1" <*> o .: "equal2"
            "not" -> ExpNot <$> o .: "bool"
            "field" -> ExpFieldBool <$> o .: "field-name"
            "literal" -> ExpLiteralBool <$> o .: "value"
            _ -> mzero
    parseJSON _ = mzero

instance ToJSON TextExp where
    toJSON (ExpFieldText x) = object
        [ "name" .= String "field"
        , "x" .= x
        ]
    toJSON (ExpLiteralText x) = object
        [ "name" .= String "literal"
        , "x" .= x
        ]
    toJSON (ExpConcat x) = object
        [ "name" .= String "concat"
        , "x" .= x
        ]
    toJSON (ExpConditional x y) = object
        [ "name" .= String "conditional"
        , "x" .= x
        , "y" .= y
        ]
instance FromJSON TextExp where
    parseJSON (Object o) = do
        name <- o .: "name"
        case name :: Text of
            "field" -> ExpFieldText <$> o .: "x"
            "literal" -> ExpLiteralText <$> o .: "x"
            "concat" -> ExpConcat <$> o .: "x"
            "conditional" -> ExpConditional <$> o .: "x" <*> o .: "y"
            _ -> mzero
    parseJSON _ = mzero
