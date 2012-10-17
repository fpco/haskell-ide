{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Text.ProjectTemplate.Types
    where

import ClassyPrelude
import Data.Typeable (Typeable)

data Project = Project
    { projectName :: Text
    , projectDescription :: Text
    , projectFields :: Vector Field
    , projectFiles :: Vector File
    }
    deriving (Show, Eq)

data Field = Field
    { fieldName :: Text
    , fieldDescription :: Text
    , fieldType :: FieldType
    }
    deriving (Show, Eq)

data FieldType = FTText (Maybe Text) -- ^ optional regex
               | FTBool
               | FTEnum (Vector EnumOption)
    deriving (Show, Eq)

data EnumOption = EnumOption
    { eoValue :: Text
    , eoDisplay :: Text
    }
    deriving (Show, Eq)

data UserValue = UVText Text | UVBool Bool
    deriving (Show, Eq, Ord, Typeable)
type UserValues = HashMap Text UserValue

data BoolExp = ExpAnd BoolExp BoolExp
             | ExpOr BoolExp BoolExp
             | ExpEq TextExp TextExp
             | ExpNot BoolExp
             | ExpFieldBool Text
             | ExpLiteralBool Bool
    deriving (Show, Eq)

data TextExp = ExpFieldText Text
             | ExpLiteralText Text
             | ExpConcat (Vector TextExp)
             | ExpConditional BoolExp TextExp
    deriving (Show, Eq)

data File = File
    { fileName :: TextExp
    , fileToGenerate :: BoolExp
    , fileContents :: FileContents
    }
    deriving (Show, Eq)

data FileContents = FileContentsText TextExp
                  | FileContentsByteString ByteString
    deriving (Show, Eq)

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

type FileOutput = HashMap Text (Either Text ByteString)
