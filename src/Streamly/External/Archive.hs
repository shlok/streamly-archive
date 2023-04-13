{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.External.Archive
  ( -- ** Read
    readArchive,

    -- ** Header
    Header,
    FileType (..),
    headerFileType,
    headerPathName,
    headerPathNameUtf8,
    headerSize,
  )
where

import Control.Exception (mask_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.Void (Void)
import Foreign (Ptr, free, malloc)
import Foreign.C.Types (CChar, CSize)
import Streamly.Data.Unfold (lmap)
import Streamly.External.Archive.Internal.Foreign
  ( Entry,
    FileType (..),
    archive_entry_filetype,
    archive_entry_pathname,
    archive_entry_pathname_utf8,
    archive_entry_size,
    archive_read_data_block,
    archive_read_free,
    archive_read_new,
    archive_read_next_header,
    archive_read_open_filename,
    archive_read_support_filter_all,
    archive_read_support_format_all,
  )
import Streamly.Internal.Data.IOFinalizer (newIOFinalizer, runIOFinalizer)
import Streamly.Internal.Data.Stream.StreamD.Type (Step (..))
import Streamly.Internal.Data.Unfold.Type (Unfold (..))

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

{-# INLINE headerSize #-}
headerSize :: Header -> IO (Maybe Int)
headerSize (Header e) = archive_entry_size e

-- | Creates an unfold with which we can stream data out of the given archive.
{-# INLINE readArchive #-}
readArchive :: (MonadIO m) => FilePath -> Unfold m Void (Either Header ByteString)
readArchive fp =
  (lmap . const) () $
    Unfold
      ( \(arch, buf, sz, offs, pos, ref, readHeader) ->
          if readHeader
            then do
              me <- liftIO $ archive_read_next_header arch
              case me of
                Nothing -> do
                  liftIO $ runIOFinalizer ref
                  return Stop
                Just e -> do
                  return $ Yield (Left $ Header e) (arch, buf, sz, offs, 0, ref, False)
            else do
              (bs, done) <- liftIO $ archive_read_data_block arch buf sz offs pos
              return $
                if B.length bs > 0
                  then
                    Yield
                      (Right bs)
                      (arch, buf, sz, offs, pos + fromIntegral (B.length bs), ref, done)
                  else Skip (arch, buf, sz, offs, pos, ref, done)
      )
      ( \() -> do
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
