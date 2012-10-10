{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
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

type FileOutput = HashMap Text (Either Text ByteString)
