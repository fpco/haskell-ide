{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Text.ProjectTemplate where

import ClassyPrelude
import Data.Typeable (Typeable)
import Data.Traversable (sequenceA)

data Project = Project
    { projectName :: Text
    , projectDescription :: Text
    , projectFields :: Vector Field
    , projectFiles :: Vector File
    }

data Field = Field
    { fieldName :: Text
    , fieldDescription :: Text
    , fieldType :: FieldType
    }

data FieldType = FTText (Maybe Text) -- ^ optional regex
               | FTBool
               | FTEnum [(Text, Text)] -- ^ value, display

data UserValue = UVText Text | UVBool Bool
    deriving (Show, Eq, Ord, Typeable)
type UserValues = HashMap Text UserValue

data Exp a where
    ExpAnd :: Exp Bool -> Exp Bool -> Exp Bool
    ExpOr :: Exp Bool -> Exp Bool -> Exp Bool
    ExpEq :: Eq a => Exp a -> Exp a -> Exp Bool
    ExpNe :: Eq a => Exp a -> Exp a -> Exp Bool
    ExpFieldText :: Text -> Exp Text
    ExpFieldBool :: Text -> Exp Bool
    ExpLiteral :: a -> Exp a

data Content = ContentText (Exp Text)
             | ContentConditional (Exp Bool) Contents
type Contents = Vector Content

data File = File
    { fileName :: Contents
    , fileToGenerate :: Exp Bool
    , fileContents :: Either Contents ByteString
    }

data EvalException = MissingUserValue Text
                   | ExpectedText Text UserValue
                   | ExpectedBool Text UserValue
    deriving (Show, Eq, Ord, Typeable)
instance Exception EvalException

type EvalExceptions = Vector EvalException

data Result a = Failure EvalExceptions | Success a
    deriving (Show, Eq, Ord, Typeable)

instance Functor Result where
    fmap _ (Failure e) = Failure e
    fmap f (Success a) = Success (f a)
instance Applicative Result where
    pure = Success
    Success f <*> Success x = Success (f x)
    Failure x <*> Failure y = Failure (x ++ y)
    Failure x <*> Success _ = Failure x
    Success _ <*> Failure y = Failure y

eval :: UserValues -> Exp a -> Result a
eval uv (ExpAnd x y) = (&&) <$> eval uv x <*> eval uv y
eval uv (ExpOr x y) = (||) <$> eval uv x <*> eval uv y
eval uv (ExpEq x y) = (==) <$> eval uv x <*> eval uv y
eval uv (ExpNe x y) = (/=) <$> eval uv x <*> eval uv y
eval uv (ExpFieldText name) =
    case lookup name uv of
        Nothing -> Failure $ singleton $ MissingUserValue name
        Just (UVText t) -> Success t
        Just v -> Failure $ singleton $ ExpectedText name v
eval uv (ExpFieldBool name) =
    case lookup name uv of
        Nothing -> Failure $ singleton $ MissingUserValue name
        Just (UVBool b) -> Success b
        Just v -> Failure $ singleton $ ExpectedBool name v
eval _ (ExpLiteral a) = Success a

evalContents :: UserValues -> Contents -> Result Text
evalContents uv =
    fmap concat . sequenceA . map go . toList
  where
    go (ContentText e) = eval uv e
    go (ContentConditional ebool contents) =
        case eval uv ebool of
            Failure x -> Failure x
            Success False -> Success empty
            Success True -> evalContents uv contents

type FileOutput = HashMap Text (Either Text ByteString)

renderFile :: UserValues -> File -> Result FileOutput
renderFile uv File{..} =
    case eval uv fileToGenerate of
        Failure x -> Failure x
        Success False -> Success empty
        Success True -> singleton
            <$> evalContents uv fileName
            <*> either (fmap Left . evalContents uv) (pure . Right) fileContents

renderProject :: UserValues -> Project -> Result FileOutput
renderProject uv Project {..} = concat . toList <$> sequenceA (map (renderFile uv) projectFiles)
