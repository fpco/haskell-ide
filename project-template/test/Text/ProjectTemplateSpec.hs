{-# LANGUAGE OverloadedStrings #-}
module Text.ProjectTemplateSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Text.ProjectTemplate
import Data.Conduit
import Control.Monad.Trans.Writer (execWriter)
import Test.QuickCheck.Arbitrary
import Data.Char (isAlphaNum)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Map as Map
import Filesystem.Path.CurrentOS (decodeString)
import Control.Arrow (second, (***))
import Control.Applicative ((<$>))
import Data.Monoid (mconcat, mappend)
import Prelude hiding (FilePath)
import Filesystem.Path (FilePath)

spec :: Spec
spec = do
    describe "create/unpack" $ do
        prop "is idempotent" $ \(Helper m) ->
            let m' =
                        execWriter
                      $ runExceptionT_
                      $ mapM_ (yield . second return) (Map.toList m)
                     $$ createTemplate
                     =$ unpackTemplate receiveMem id
                m'' = Map.fromList $ map (second $ mconcat . L.toChunks) $ Map.toList m'
             in if m == m'' then True else error (show m'')
    describe "binaries" $ do
        prop "works with multilines" $ \words' ->
            let bs = S.pack words'
                encoded = B64.joinWith "\n" 5 $ B64.encode bs
                content = "{-# START_FILE BASE64 foo #-}\n" `mappend` encoded
                m = execWriter $ runExceptionT_ $ yield content $$ unpackTemplate receiveMem id
             in Map.lookup "foo" m == Just (L.fromChunks [bs])

newtype Helper = Helper (Map.Map FilePath S.ByteString)
    deriving (Show, Eq)

instance Arbitrary Helper where
    arbitrary =
        Helper . Map.fromList <$> mapM (const $ (decodeString . def "foo" . filter isAlphaNum *** S.pack . def (S.unpack "bar")) <$> arbitrary) [1..10 :: Int]
      where
        def x y
            | null y = x
            | otherwise = y
