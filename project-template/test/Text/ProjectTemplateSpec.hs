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
            -- ignore posix end newline
                trimEnd s = case S.stripSuffix "\n" s of
                                Nothing -> s
                                Just ss -> trimEnd ss
                trimBodyEnd = Map.map trimEnd
            trimBodyEnd m `shouldBe` trimBodyEnd m''
    describe "binaries" $ do
        prop "works with multilines" $ \words' -> do
            let bs = S.pack words'
                encoded = B64.joinWith "\n" 5 $ B64.encode bs
                content = "{-# START_FILE BASE64 foo #-}\n" `mappend` encoded
            m <- execWriterT $ runConduit $ yield content .| unpackTemplate receiveMem id
            Map.lookup "foo" m `shouldBe` Just (L.fromChunks [bs])
        prop "works with multifile" $ \words' -> do
            let bs = S.pack words'
                encoded = B64.joinWith "\n" 5 $ B64.encode bs
                fooContent = "{-# START_FILE BASE64 foo #-}\n" `mappend` encoded
                barContent = "{-# START_FILE BASE64 bar #-}\n" `mappend` encoded
            m <- execWriterT $ runConduit $
                (yield fooContent >> yield barContent) .| unpackTemplate receiveMem id
            Map.lookup "foo" m `shouldBe` Just (L.fromChunks [bs])
            Map.lookup "bar" m `shouldBe` Just (L.fromChunks [bs])
    describe "POSIX file end of newline" $
        it "new-template" $ do
            newTemplate <- S.readFile "test/new-template.hsfiles"
            m <- execWriterT $ runConduit $ yield newTemplate .| unpackTemplate receiveMem id
            ("\n" `L.isSuffixOf`) <$> Map.elems m `shouldSatisfy` and
            -- don't repeat newline
            ("\n\n" `L.isSuffixOf`) <$> Map.elems m `shouldSatisfy` not . or

newtype Helper = Helper (Map.Map FilePath S.ByteString)
    deriving (Show, Eq)

instance Arbitrary Helper where
    arbitrary =
        Helper . Map.fromList <$> mapM (const $ (def "foo" . filter isAlphaNum *** S.pack . def (S.unpack "bar")) <$> arbitrary) [1..10 :: Int]
      where
        def x y
            | null y = x
            | otherwise = y
