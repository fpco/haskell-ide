{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
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

import           ClassyPrelude.Conduit
import           Control.Monad.Writer      (MonadWriter, tell)
import qualified Data.ByteString.Base64    as B64
import           Data.Typeable             (Typeable)
import           Filesystem                (createTree)
import           Filesystem.Path.CurrentOS (directory, encode, fromText)

-- | Create a template file from a stream of file/contents combinations.
--
-- Since 0.1.0
createTemplate
    :: Monad m
    => GInfConduit (FilePath, m ByteString) m ByteString
createTemplate = awaitForever $ \(fp, getBS) -> do
    bs <- lift getBS
    case runException $ yield bs $$ decodeUtf8 =$ sinkNull of
        Left{} -> do
            yield "{-# START_FILE BASE64 "
            yield $ encode fp
            yield " #-}\n"
            yield $ B64.encode bs
            yield "\n"
        Right{} -> do
            yield "{-# START_FILE "
            yield $ encode fp
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
    => (FilePath -> Sink ByteString m ()) -- ^ receive individual files
    -> (Text -> Text) -- ^ fix each input line, good for variables
    -> Sink ByteString m ()
unpackTemplate perFile fixLine =
    decodeUtf8 =$ lines =$ map fixLine =$ start
  where
    start =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Nothing -> lift $ monadThrow $ InvalidInput t
                Just (fp', isBinary) -> do
                    let src
                            | isBinary  = binaryLoop
                            | otherwise = textLoop True
                    src =$ perFile (fromText fp')
                    start

    binaryLoop = do
        await >>= maybe (lift $ monadThrow BinaryLoopNeedsOneLine) go
      where
        go = yield . B64.decodeLenient . encodeUtf8
    textLoop isFirst =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Just{} -> leftover t
                Nothing -> do
                    unless isFirst $ yield "\n"
                    yield $ encodeUtf8 t
                    textLoop False

    getFileName t =
        case words t of
            ["{-#", "START_FILE", fn, "#-}"] -> Just (fn, False)
            ["{-#", "START_FILE", "BASE64", fn, "#-}"] -> Just (fn, True)
            _ -> Nothing

-- | The first argument to 'unpackTemplate', specifying how to receive a file.
--
-- Since 0.1.0
type FileReceiver m = FilePath -> Sink ByteString m ()

-- | Receive files to the given folder on the filesystem.
--
-- > unpackTemplate (receiveFS "some-destination") (T.replace "PROJECTNAME" "foo")
--
-- Since 0.1.0
receiveFS :: MonadResource m
          => FilePath -- ^ root
          -> FileReceiver m
receiveFS root rel = do
    liftIO $ createTree $ directory fp
    writeFile fp
  where
    fp = root </> rel

-- | Receive files to a @Writer@ monad in memory.
--
-- > execWriter $ runExceptionT_ $ src $$ unpackTemplate receiveMem id
--
-- Since 0.1.0
receiveMem :: MonadWriter (Map FilePath LByteString) m
           => FileReceiver m
receiveMem fp = do
    bss <- consume
    lift $ tell $ singleton fp $ fromChunks bss

-- | Exceptions that can be thrown.
--
-- Since 0.1.0
data ProjectTemplateException = InvalidInput Text
                              | BinaryLoopNeedsOneLine
    deriving (Show, Typeable)
instance Exception ProjectTemplateException
