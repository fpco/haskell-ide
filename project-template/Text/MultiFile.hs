{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Text.MultiFile where

import           Control.Monad                (unless)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Resource (runExceptionT)
import           Control.Monad.Writer         (MonadWriter, tell)
import qualified Data.ByteString              as S
import qualified Data.ByteString.Base64       as B64
import qualified Data.ByteString.Lazy         as L
import           Data.Conduit                 (Conduit, MonadResource, Sink,
                                               await, awaitForever, leftover,
                                               yield, ($$), (=$))
import           Data.Conduit.Binary          (sinkFile)
import           Data.Conduit.List            (sinkNull)
import qualified Data.Conduit.List            as CL
import qualified Data.Conduit.Text            as CT
import           Data.Functor.Identity        (runIdentity)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (encodeUtf8)
import           Filesystem                   (createTree)
import           Filesystem.Path.CurrentOS    (FilePath, directory, encode,
                                               encodeString, fromText, (</>))
import           Prelude                      hiding (FilePath)

fsPerFile :: MonadResource m
          => FilePath -- ^ root
          -> FilePath -- ^ current relpath
          -> Sink S.ByteString m ()
fsPerFile root rel = do
    liftIO $ createTree $ directory fp
    sinkFile $ encodeString fp
  where
    fp = root </> rel

memPerFile :: MonadWriter (Map.Map FilePath L.ByteString) m
           => FilePath -- ^ current relpath
           -> Sink S.ByteString m ()
memPerFile fp = do
    bss <- CL.consume
    lift $ tell $ Map.singleton fp $ L.fromChunks bss

unpackMultiFile
    :: MonadResource m
    => (FilePath -> Sink S.ByteString m ()) -- ^ receive individual files
    -> (Text -> Text) -- ^ fix each input line, good for variables
    -> Sink S.ByteString m ()
unpackMultiFile perFile fixLine =
    CT.decode CT.utf8 =$ CT.lines =$ CL.map fixLine =$ start
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
        case T.words t of
            ["{-#", "START_FILE", fn, "#-}"] -> Just (fn, False)
            ["{-#", "START_FILE", "BASE64", fn, "#-}"] -> Just (fn, True)
            _ -> Nothing

createMultiFile
    :: MonadIO m
    => FilePath -- ^ folder containing the files
    -> Conduit FilePath m S.ByteString -- ^ FilePath is relative to containing folder
createMultiFile root = do
    awaitForever handleFile
  where
    handleFile fp' = do
        bs <- liftIO $ S.readFile $ encodeString fp
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
