{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.External.Archive
  ( -- ** Read
    readArchive,
    groupByHeader,

    -- ** Header
    Header,
    FileType (..),
    headerFileType,
    headerPathName,
    headerPathNameUtf8,
    headerSize,
  )
where

import Control.Exception
import Control.Monad.IO.Class
import Data.ByteString
import qualified Data.ByteString as B
import Data.Either
import Data.Function
import Foreign
import Foreign.C.Types
import Streamly.Data.Fold
import qualified Streamly.Data.Parser as P
import Streamly.Data.Stream.Prelude
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.Data.Unfold
import Streamly.External.Archive.Internal.Foreign
import Streamly.Internal.Data.IOFinalizer
import qualified Streamly.Internal.Data.Unfold as U

-- | Header information for an entry in the archive.
newtype Header = Header Entry

{-# INLINE headerFileType #-}
headerFileType :: Header -> IO (Maybe FileType)
headerFileType (Header e) = archive_entry_filetype e

{-# INLINE headerPathName #-}
headerPathName :: Header -> IO (Maybe ByteString)
headerPathName (Header e) = archive_entry_pathname e

{-# INLINE headerPathNameUtf8 #-}
headerPathNameUtf8 :: Header -> IO (Maybe ByteString)
headerPathNameUtf8 (Header e) = archive_entry_pathname_utf8 e

-- | Returns the file size of the entry, if it has been set; returns 'Nothing' otherwise.
{-# INLINE headerSize #-}
headerSize :: Header -> IO (Maybe Int)
headerSize (Header e) = archive_entry_size e

-- | A convenience function for grouping @"Either Header ByteString"@s, usually obtained with
-- 'readArchive', by the headers. The input @Fold@ processes a single entry (a 'Header' followed by
-- zero or more @ByteString@s).
{-# INLINE groupByHeader #-}
groupByHeader ::
  (Monad m) =>
  Fold m (Either Header ByteString) b ->
  Stream m (Either Header ByteString) ->
  Stream m b
groupByHeader itemFold str =
  str
    & S.parseMany (P.groupBy (\_ e -> isRight e) itemFold)
    & fmap
      ( \case
          Left _ ->
            -- groupBy is documented to never fail.
            error "unexpected parseMany/groupBy error"
          Right b -> b
      )

-- | Creates an unfold with which we can stream data out of the given archive. For each entry in the
-- archive, we get a 'Header' followed by zero or more @ByteString@s containing chunks of file data.
{-# INLINE readArchive #-}
readArchive :: (MonadIO m) => Unfold m FilePath (Either Header ByteString)
readArchive =
  U.Unfold
    ( \(arch, buf, sz, offs, pos, ref, readHeader) ->
        if readHeader
          then do
            me <- liftIO $ archive_read_next_header arch
            case me of
              Nothing -> do
                liftIO $ runIOFinalizer ref
                return U.Stop
              Just e -> do
                return $ U.Yield (Left $ Header e) (arch, buf, sz, offs, 0, ref, False)
          else do
            (bs, done) <- liftIO $ archive_read_data_block arch buf sz offs pos
            return $
              if B.length bs > 0
                then
                  U.Yield
                    (Right bs)
                    (arch, buf, sz, offs, pos + fromIntegral (B.length bs), ref, done)
                else U.Skip (arch, buf, sz, offs, pos, ref, done)
    )
    ( \fp -> do
        (arch, buf, sz, offs, ref) <- liftIO . mask_ $ do
          arch <- liftIO archive_read_new
          buf :: Ptr (Ptr CChar) <- liftIO malloc
          sz :: Ptr CSize <- liftIO malloc
          offs :: Ptr Int64 <- liftIO malloc
          ref <- newIOFinalizer $ archive_read_free arch >> free buf >> free sz >> free offs
          return (arch, buf, sz, offs, ref)
        liftIO $ archive_read_support_filter_all arch
        liftIO $ archive_read_support_format_all arch
        liftIO $ archive_read_open_filename arch fp
        return (arch, buf, sz, offs, 0, ref, True)
    )
