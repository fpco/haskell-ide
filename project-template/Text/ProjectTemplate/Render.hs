{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Text.ProjectTemplate.Render where

import ClassyPrelude
import Data.Traversable (sequenceA)
import Text.ProjectTemplate.Types

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
