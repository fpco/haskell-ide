{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
module Text.ProjectTemplate where

import           ClassyPrelude.Conduit
import           Control.Monad                (unless)
import           Control.Monad.Trans.Resource (runExceptionT)
import           Control.Monad.Writer         (MonadWriter, tell)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Base64       as B64
import           Data.Conduit.Binary          (sinkFile)
import           Data.Conduit.List            (sinkNull)
import           Data.Conduit.List            (consume)
import qualified Data.Conduit.Text            as CT
import           Data.Functor.Identity        (runIdentity)
import           Data.Text.Encoding           (encodeUtf8)
import           Filesystem                   (createTree)
import           Filesystem.Path.CurrentOS    (directory, encode, encodeString,
                                               fromText)

fsPerFile :: MonadResource m
          => FilePath -- ^ root
          -> FilePath -- ^ current relpath
          -> Sink ByteString m ()
fsPerFile root rel = do
    liftIO $ createTree $ directory fp
    sinkFile $ encodeString fp
  where
    fp = root </> rel

memPerFile :: MonadWriter (Map FilePath LByteString) m
           => FilePath -- ^ current relpath
           -> Sink ByteString m ()
memPerFile fp = do
    bss <- consume
    lift $ tell $ singleton fp $ fromChunks bss

unpackMultiFile
    :: MonadResource m
    => (FilePath -> Sink ByteString m ()) -- ^ receive individual files
    -> (Text -> Text) -- ^ fix each input line, good for variables
    -> Sink ByteString m ()
unpackMultiFile perFile fixLine =
    CT.decode CT.utf8 =$ CT.lines =$ map fixLine =$ start
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
                            | otherwise = textLoop
                    src =$ perFile (fromText fp')
                    start

    binaryLoop = do
        await >>= maybe (error "binaryLoop needs 1 line") go
      where
        go = yield . B64.decodeLenient . encodeUtf8
    textLoop =
        await >>= maybe (return ()) go
      where
        go t =
            case getFileName t of
                Just{} -> leftover t
                Nothing -> do
                    yield $ encodeUtf8 t
                    yield "\n"
                    textLoop

    getFileName t =
        case words t of
            ["{-#", "START_FILE", fn, "#-}"] -> Just (fn, False)
            ["{-#", "START_FILE", "BASE64", fn, "#-}"] -> Just (fn, True)
            _ -> Nothing

createMultiFile
    :: MonadIO m
    => FilePath -- ^ folder containing the files
    -> Conduit FilePath m ByteString -- ^ FilePath is relative to containing folder
createMultiFile root = do
    awaitForever handleFile
  where
    handleFile fp' = do
        bs <- readFile fp
        case runIdentity $ runExceptionT $ yield bs $$ CT.decode CT.utf8 =$ sinkNull of
            Left{} -> do
                yield "{-# START_FILE BASE64 "
                yield $ encode fp'
                yield " #-}\n"
                yield $ B64.encode bs
                yield "\n"
            Right{} -> do
                yield "{-# START_FILE "
                yield $ encode fp'
                yield " #-}\n"
                yield bs
                unless ("\n" `S.isSuffixOf` bs) $ yield "\n"
      where
        fp = root </> fp'
