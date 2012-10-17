{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module Text.ProjectTemplate.Render where

import ClassyPrelude
import Data.Traversable (sequenceA)
import Text.ProjectTemplate.Types

evalBool :: UserValues -> BoolExp -> Result Bool
evalBool uv (ExpAnd x y) = (&&) <$> evalBool uv x <*> evalBool uv y
evalBool uv (ExpOr x y) = (||) <$> evalBool uv x <*> evalBool uv y
evalBool uv (ExpEq x y) = (==) <$> evalText uv x <*> evalText uv y
evalBool uv (ExpNot x) = (not) <$> evalBool uv x
evalBool uv (ExpFieldBool name) =
    case lookup name uv of
        Nothing -> Failure $ singleton $ MissingUserValue name
        Just (UVBool b) -> Success b
        Just v -> Failure $ singleton $ ExpectedBool name v
evalBool _ (ExpLiteralBool a) = Success a

evalText :: UserValues -> TextExp -> Result Text
evalText uv (ExpFieldText name) =
    case lookup name uv of
        Nothing -> Failure $ singleton $ MissingUserValue name
        Just (UVText t) -> Success t
        Just v -> Failure $ singleton $ ExpectedText name v
evalText _ (ExpLiteralText a) = Success a
evalText uv (ExpConcat es) = concat <$> sequenceA (map (evalText uv) $ toList es)
evalText uv (ExpConditional ebool et) =
    go <$> evalBool uv ebool <*> evalText uv et
  where
    go True x = x
    go False _ = empty

renderFile :: UserValues -> File -> Result FileOutput
renderFile uv File{..} =
    case evalBool uv fileToGenerate of
        Failure x -> Failure x
        Success False -> Success empty
        Success True -> singleton
            <$> evalText uv fileName
            <*> (case fileContents of
                    FileContentsText t -> Left <$> evalText uv t
                    FileContentsByteString bs -> pure $ Right bs)

renderProject :: UserValues -> Project -> Result FileOutput
renderProject uv Project {..} = concat . toList <$> sequenceA (map (renderFile uv) projectFiles)
