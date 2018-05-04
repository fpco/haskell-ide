{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module Text.ProjectTemplate
    ( -- * Create a template
      createTemplate
      -- * Unpack a template
    , unpackTemplate
      -- ** Receivers
    , FileReceiver
    , receiveMem
    , receiveFS
      -- * Exceptions
    , ProjectTemplateException (..)
    ) where

import           Control.Exception            (Exception, assert)
import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource (MonadResource, MonadThrow,
                                               throwM)
import           Control.Monad.Writer         (MonadWriter, tell)
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Base64       as B64
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit                 (ConduitM, await,
                                               awaitForever, leftover, yield,
                                               runConduit, (.|))
import qualified Data.Conduit.Binary          as CB
import           Data.Conduit.List            (consume, sinkNull)
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import           Data.Typeable                (Typeable)
import           Data.Void                    (Void)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              (takeDirectory, (</>))

-- | Create a template file from a stream of file/contents combinations.
--
-- Since 0.1.0
createTemplate
    :: Monad m => ConduitM (FilePath, m ByteString) ByteString m ()
createTemplate = awaitForever $ \(fp, getBS) -> do
    bs <- lift getBS
    case runConduit $ yield bs .| CT.decode CT.utf8 .| sinkNull of
        Nothing -> do
            yield "{-# START_FILE BASE64 "
            yield $ encodeUtf8 $ T.pack fp
            yield " #-}\n"
            yield $ B64.joinWith "\n" 76 $ B64.encode bs
            yield "\n"
        Just _ -> do
            yield "{-# START_FILE "
            yield $ encodeUtf8 $ T.pack fp
            yield " #-}\n"
            yield bs
            yield "\n"

-- | Unpack a template to some destination. Destination is provided by the
-- first argument.
--
-- The second argument allows you to modify the incoming stream, usually to
-- replace variables. For example, to replace PROJECTNAME with myproject, you
-- could use:
--
-- > Data.Text.replace "PROJECTNAME" "myproject"
--
-- Note that this will affect both file contents and file names.
--
-- Since 0.1.0
unpackTemplate
    :: MonadThrow m
    => (FilePath -> ConduitM ByteString o m ()) -- ^ receive individual files
    -> (Text -> Text) -- ^ fix each input line, good for variables
    -> ConduitM ByteString o m ()
unpackTemplate perFile fixLine =
    CT.decode CT.utf8 .| CT.lines .| CL.map fixLine .| start
  where
    start =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Nothing -> lift $ throwM $ InvalidInput t
                Just (fp', isBinary) -> do
                    let src
                            | isBinary  = binaryLoop .| decode64
                            | otherwise = textLoop True
                    src .| perFile (T.unpack fp')
                    start

    binaryLoop = do
        await >>= maybe (yield "\n") go
      where
        go t =
            case getFileName t of
                Just{} -> leftover t
                Nothing -> do
                    yield $ encodeUtf8 t
                    binaryLoop
    textLoop isFirst =
        await >>= maybe (yield "\n") go
      where
        go t =
            case getFileName t of
                Just{} -> leftover t
                Nothing -> do
                    unless isFirst $ yield "\n"
                    yield $ encodeUtf8 t
                    textLoop False

    getFileName t =
        case T.words t of
            ["{-#", "START_FILE", fn, "#-}"] -> Just (fn, False)
            ["{-#", "START_FILE", "BASE64", fn, "#-}"] -> Just (fn, True)
            _ -> Nothing

-- | The first argument to 'unpackTemplate', specifying how to receive a file.
--
-- Since 0.1.0
type FileReceiver m = FilePath -> ConduitM ByteString Void m ()

-- | Receive files to the given folder on the filesystem.
--
-- > unpackTemplate (receiveFS "some-destination") (T.replace "PROJECTNAME" "foo")
--
-- Since 0.1.0
receiveFS :: MonadResource m
          => FilePath -- ^ root
          -> FileReceiver m
receiveFS root rel = do
    liftIO $ createDirectoryIfMissing True $ takeDirectory fp
    CB.sinkFile fp
  where
    fp = root </> rel

-- | Receive files to a @Writer@ monad in memory.
--
-- > execWriter $ runExceptionT_ $ src $$ unpackTemplate receiveMem id
--
-- Since 0.1.0
receiveMem :: MonadWriter (Map FilePath L.ByteString) m
           => FileReceiver m
receiveMem fp = do
    bss <- consume
    lift $ tell $ Map.singleton fp $ L.fromChunks bss

-- | Exceptions that can be thrown.
--
-- Since 0.1.0
data ProjectTemplateException = InvalidInput Text
                              | BinaryLoopNeedsOneLine
    deriving (Show, Typeable)
instance Exception ProjectTemplateException

decode64 :: Monad m => ConduitM ByteString ByteString m ()
decode64 = codeWith 4 B64.decodeLenient

codeWith :: Monad m => Int -> (ByteString -> ByteString) -> ConduitM ByteString ByteString m ()
codeWith size f =
    loop
  where
    loop = await >>= maybe (return ()) push

    loopWith bs
        | S.null bs = loop
        | otherwise = await >>= maybe (yield (f bs)) (pushWith bs)

    push bs = do
        let (x, y) = S.splitAt (len - (len `mod` size)) bs
        unless (S.null x) $ yield $ f x
        loopWith y
      where
        len = S.length bs

    pushWith bs1 bs2 | S.length bs1 + S.length bs2 < size = loopWith (S.append bs1 bs2)
    pushWith bs1 bs2 = assertion1 $ assertion2 $ do
        yield $ f bs1'
        push y
      where
        m = S.length bs1 `mod` size
        (x, y) = S.splitAt (size - m) bs2
        bs1' = S.append bs1 x

        assertion1 = assert $ S.length bs1 < size
        assertion2 = assert $ S.length bs1' `mod` size == 0
