{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Text.ProjectTemplateSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Text.ProjectTemplate
import ClassyPrelude.Conduit
import Control.Monad.Trans.Writer (execWriter)
import Test.QuickCheck.Arbitrary
import Data.Char (isAlphaNum)

spec :: Spec
spec = do
    describe "create/unpack" $ do
        prop "is idempotent" $ \(Helper m) ->
            let m' =
                        execWriter
                      $ runExceptionT_
                      $ mapM_ (yield . second return) (unpack m)
                     $$ createTemplate
                     =$ unpackTemplate receiveMem id
                m'' = pack $ map (second $ concat . toChunks) $ unpack m'
             in if m == m'' then True else error (show m'')

newtype Helper = Helper (Map FilePath ByteString)
    deriving (Show, Eq)

instance Arbitrary Helper where
    arbitrary =
        Helper . pack <$> mapM (const $ (pack . def "foo" . filter isAlphaNum *** pack . def (unpack $ asByteString "bar")) <$> arbitrary) [1..10 :: Int]
      where
        def x y
            | null y = x
            | otherwise = y
