{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.External.Archive
  ( -- ** Read
    readArchive,
    groupByLefts,

    -- *** Read options
    ReadOptions,
    mapHeaderMaybe,

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
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Either
import Data.Function
import Foreign
import Foreign.C.Types
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Parser as P
import Streamly.Data.Stream.Prelude (Stream)
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

-- | Creates an unfold with which we can stream data out of the given archive.
--
-- By default (with 'id' as the read options modifier), we get for each entry in the archive a
-- 'Header' followed by zero or more @ByteString@s containing chunks of file data.
--
-- To modify the read options, one can use function composition.
{-# INLINE readArchive #-}
readArchive ::
  (MonadIO m) =>
  Unfold m (ReadOptions m Header -> ReadOptions m a, FilePath) (Either a ByteString)
readArchive =
  U.Unfold
    ( \(ropts, arch, buf, sz, offs, pos, ref, readHeader) ->
        if readHeader
          then do
            me <- liftIO $ archive_read_next_header arch
            case me of
              Nothing -> do
                liftIO $ runIOFinalizer ref
                return U.Stop
              Just e -> do
                let hdr = Header e
                m <- _mapHeaderMaybe ropts hdr
                return $ case m of
                  Nothing -> U.Skip (ropts, arch, buf, sz, offs, 0, ref, True)
                  Just a -> U.Yield (Left a) (ropts, arch, buf, sz, offs, 0, ref, False)
          else do
            (bs, done) <- liftIO $ archive_read_data_block arch buf sz offs pos
            return $
              if B.length bs > 0
                then
                  U.Yield
                    (Right bs)
                    (ropts, arch, buf, sz, offs, pos + fromIntegral (B.length bs), ref, done)
                else U.Skip (ropts, arch, buf, sz, offs, pos, ref, done)
    )
    ( \(modifier, fp) -> do
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

        -- + We ended up with functions instead of records to avoid an error about an ambiguous
        --   monad type for defaultReadOptions when the user sets the headerFilter record.
        -- + (A dummy Proxy record worked too, but partially exporting records breaks Haddock.)
        let ropts = modifier _defaultReadOptions

        return (ropts, arch, buf, sz, offs, 0, ref, True)
    )

newtype ReadOptions m a = ReadOptions
  { _mapHeaderMaybe :: Header -> m (Maybe a)
  }

_defaultReadOptions :: (Monad m) => ReadOptions m Header
_defaultReadOptions =
  ReadOptions
    { _mapHeaderMaybe = return . Just
    }

-- | If this returns @Just@ for a header, that header (mapped to a different value if desired) and
-- any following @ByteString@ chunks are included in the 'readArchive' unfold. If this returns
-- @Nothing@ for a header, that header and any following @ByteString@ chunks are excluded from the
-- 'readArchive' unfold.
--
-- By default, all entries are included with unaltered headers.
mapHeaderMaybe :: (Header -> m (Maybe a)) -> ReadOptions m Header -> ReadOptions m a
mapHeaderMaybe x o = o {_mapHeaderMaybe = x}

-- | A utility function for grouping @Either@s by the @Left@s. The input @Fold@ processes a single
-- @Left@ followed by any subsequent (zero or more) @Right@s.
{-# INLINE groupByLefts #-}
groupByLefts ::
  (Monad m) =>
  Fold m (Either a b) c ->
  Stream m (Either a b) ->
  Stream m c
groupByLefts itemFold str =
  str
    & S.parseMany (P.groupBy (\_ e -> isRight e) itemFold)
    & fmap
      ( \case
          Left _ ->
            -- groupBy is documented to never fail.
            error "unexpected parseMany/groupBy error"
          Right c -> c
      )
