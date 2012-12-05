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
import qualified Data.ByteString.Base64 as B64

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
    describe "binaries" $ do
        prop "works with multilines" $ \words ->
            let bs = pack words
                encoded = B64.joinWith "\n" 5 $ B64.encode bs
                content = "{-# START_FILE BASE64 foo #-}\n" ++ encoded
                m = execWriter $ runExceptionT_ $ yield content $$ unpackTemplate receiveMem id
             in lookup "foo" m == Just (fromChunks [bs])

newtype Helper = Helper (Map FilePath ByteString)
    deriving (Show, Eq)

instance Arbitrary Helper where
    arbitrary =
        Helper . pack <$> mapM (const $ (pack . def "foo" . filter isAlphaNum *** pack . def (unpack $ asByteString "bar")) <$> arbitrary) [1..10 :: Int]
      where
        def x y
            | null y = x
            | otherwise = y
