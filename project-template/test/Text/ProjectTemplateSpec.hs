{-# LANGUAGE OverloadedStrings #-}
module Text.ProjectTemplateSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Text.ProjectTemplate
import Data.Conduit
import Control.Monad.Trans.Writer (execWriterT)
import Test.QuickCheck.Arbitrary
import Data.Char (isAlphaNum)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Map as Map
import Control.Arrow (second, (***))
import Control.Applicative ((<$>))
import Data.Monoid (mconcat, mappend)

spec :: Spec
spec = do
    describe "create/unpack" $ do
        prop "is idempotent" $ \(Helper m) -> do
            m' <-
                        execWriterT
                      $ runConduit
                      $ mapM_ (yield . second return) (Map.toList m)
                     .| createTemplate
                     .| unpackTemplate receiveMem id
            let m'' = Map.fromList $ map (second $ mconcat . L.toChunks) $ Map.toList m'
            m `shouldBe` m''
    describe "binaries" $ do
        prop "works with multilines" $ \words' -> do
            let bs = S.pack words'
                encoded = joinWith "\n" 5 $ B64.encode bs
                content = "{-# START_FILE BASE64 foo #-}\n" `mappend` encoded
            m <- execWriterT $ runConduit $ yield content .| unpackTemplate receiveMem id
            Map.lookup "foo" m `shouldBe` Just (L.fromChunks [bs])

joinWith :: S.ByteString -> Int -> S.ByteString -> S.ByteString
joinWith joiner size = S.concat . map (`S.append` joiner) . chunksOf size

chunksOf :: Int -> S.ByteString -> [S.ByteString]
chunksOf _ bs | S.null bs = []
chunksOf size bs =
    let (x, y) = S.splitAt size bs
     in x : chunksOf size y

newtype Helper = Helper (Map.Map FilePath S.ByteString)
    deriving (Show, Eq)

instance Arbitrary Helper where
    arbitrary =
        Helper . Map.fromList <$> mapM (const $ (def "foo" . filter isAlphaNum *** S.pack . def (S.unpack "bar")) <$> arbitrary) [1..10 :: Int]
      where
        def x y
            | null y = x
            | otherwise = y
