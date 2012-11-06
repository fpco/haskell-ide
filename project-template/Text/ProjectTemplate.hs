{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Text.ProjectTemplate where

import           ClassyPrelude.Conduit
import           Control.Monad.Writer         (MonadWriter, tell)
import qualified Data.ByteString.Base64       as B64
import           Data.Functor.Identity        (runIdentity)
import           Filesystem                   (createTree)
import           Filesystem.Path.CurrentOS    (directory, encode, fromText)

fsPerFile :: MonadResource m
          => FilePath -- ^ root
          -> FilePath -- ^ current relpath
          -> Sink ByteString m ()
fsPerFile root rel = do
    liftIO $ createTree $ directory fp
    writeFile fp
  where
    fp = root </> rel

memPerFile :: MonadWriter (Map FilePath LByteString) m
           => FilePath -- ^ current relpath
           -> Sink ByteString m ()
memPerFile fp = do
    bss <- consume
    lift $ tell $ singleton fp $ fromChunks bss

unpackMultiFile
    :: MonadThrow m
    => (FilePath -> Sink ByteString m ()) -- ^ receive individual files
    -> (Text -> Text) -- ^ fix each input line, good for variables
    -> Sink ByteString m ()
unpackMultiFile perFile fixLine =
    decodeUtf8 =$ lines =$ map fixLine =$ start
  where
    start =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Nothing -> error $ "Invalid input: " ++ show t
                Just (fp', isBinary) -> do
                    let src
                            | isBinary  = binaryLoop
                            | otherwise = textLoop True
                    src =$ perFile (fromText fp')
                    start

    binaryLoop = do
        await >>= maybe (error "binaryLoop needs 1 line") go
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

loadFromFS :: MonadIO m
           => FilePath -- ^ root folder
           -> FilePath -- ^ relative path
           -> (FilePath, m ByteString)
loadFromFS root rel =
    (fp, readFile fp)
  where
    fp = root </> rel

createMultiFile
    :: Monad m
    => Conduit (FilePath, m ByteString) m ByteString
createMultiFile = awaitForever $ \(fp, getBS) -> do
    bs <- lift getBS
    case runIdentity $ runExceptionT $ yield bs $$ decodeUtf8 =$ sinkNull of
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
