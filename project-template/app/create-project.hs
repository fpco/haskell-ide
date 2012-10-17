{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
import ClassyPrelude.Conduit
import qualified Data.Aeson as J
import Text.ProjectTemplate
import Filesystem (canonicalizePath)
import Data.Conduit.Filesystem (traverse)
import Control.Monad.Trans.Resource (runExceptionT)
import Data.Functor.Identity (runIdentity)
import qualified Data.Conduit.Text as CT
import Filesystem.Path.CurrentOS (toText)
import qualified Data.Text as T
import Data.List (intersperse)

fieldMap = asMap $ pack
    [ ("SOMEPROJECTNAME", "project-name")
    , ("SOME_DEVELOPER", "user-name")
    ]

main :: IO ()
main = do
    (root, output) <- readArgs
    project <- mkProject root
    writeFile output $ J.encode project

mkProject :: FilePath -> IO Project
mkProject root' = do
    root <- (</> "") <$> canonicalizePath root'
    files <- traverse False root
          $$ mapM (parseFile root)
          =$ fold (\x y -> x ++ singleton y) empty
    return Project
        { projectName = "Yesod scaffolding"
        , projectDescription = "Sample Yesod scaffolding"
        , projectFields = pack
            [ Field "user-name" "Your name" $ FTText Nothing
            , Field "project-name" "Name of project" $ FTText Nothing
            ]
        , projectFiles = files
        }

parseFile :: FilePath -- ^ root folder
          -> FilePath -- ^ individual file
          -> IO File
parseFile root fp = do
    rel <-
        case stripPrefix root fp of
            Just x -> return x
            Nothing -> error "Couldn't get relative path"
    bs <- readFile fp
    et <-    runExceptionT
           $ yield bs
          $$ CT.decode CT.utf8
          =$ fold (++) empty
    contents <-
        case et of
            Left e -> return $ FileContentsByteString bs
            Right t -> FileContentsText <$> toExp t
    name <- toExp $ either id id $ toText rel
    return File
        { fileName = name
        , fileToGenerate = ExpLiteralBool True
        , fileContents = contents
        }
  where
    toExp = goFields (unpack fieldMap) . ExpLiteralText
    goFields [] e = return e
    goFields ((t, f):fs) e = goField t f e >>= goFields fs

    goField _ _ e@ExpFieldText{} = return e
    goField t f (ExpLiteralText orig) = wrap <$> goText t f orig
    goField t f (ExpConcat es) = ExpConcat <$> mapM (goField t f) es
    goField t f (ExpConditional b e) = ExpConditional b <$> goField t f e

    wrap [x] = x
    wrap xs = ExpConcat $ pack xs

    goText t f orig = return $ intersperse (ExpFieldText f) $ map ExpLiteralText $ T.splitOn t orig
