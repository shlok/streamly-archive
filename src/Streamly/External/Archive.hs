{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.External.Archive
  ( -- ** Read
    readArchive,

    -- *** Read options
    ReadOptions,
    mapHeaderMaybe,

    -- ** Utility functions

    -- | Various utility functions that some might find useful.
    groupByLefts,
    chunkOn,
    chunkOnFold,

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
import Data.Foldable
import Data.Function
import qualified Data.Sequence as Seq
import Foreign
import Foreign.C.Types
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Parser as P
import Streamly.Data.Stream.Prelude (Stream)
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.Data.Unfold
import Streamly.External.Archive.Internal.Foreign
import qualified Streamly.Internal.Data.Fold as F
import Streamly.Internal.Data.IOFinalizer
import qualified Streamly.Internal.Data.Stream as S
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

-- | The state of the chunkOn stream.
data ChunkOnState_ is h
  = -- | The initial state; or a header is done being yielded.
    COInitOrYieldHeader_
  | -- | A bytestring not containing splitWd is being built up.
    COResidue_ !ByteString
  | -- | Chunks are being processed.
    COProcessChunks_ ![ByteString] !ByteString
  | -- | A stop has been asked for.
    COStop_
  | -- | A header yield has been asked for.
    COYieldHeader_ !h !is

-- | Chunks up the bytestrings following each @Left@ by the given word, discarding the given word.
-- (For instance, the word could be @10@ (newline), which gives us lines as the chunks.) The
-- bytestrings in the resulting stream are the desired chunks.
{-# INLINE chunkOn #-}
chunkOn ::
  (Monad m) =>
  Word8 ->
  Stream m (Either a ByteString) ->
  Stream m (Either a ByteString)
chunkOn splitWd (S.Stream istep isinit) =
  -- "i": input.
  S.Stream step' (isinit, COInitOrYieldHeader_)
  where
    -- A utility function to obtain (chunks, next residue) from the previous residue and the latest
    -- incoming bytestring.
    {-# INLINE toChunks #-}
    toChunks residue newbs =
      -- Non-empty newbs expected.
      let tentativeChunks = Seq.fromList . B.split splitWd $ residue `B.append` newbs
       in case tentativeChunks of
            Seq.Empty -> (Seq.empty, "")
            init' Seq.:|> last' ->
              -- Note: This logic works also when newbs ends with splitWd because then the last
              -- chunk is the empty bytestring.
              (init', last')

    -- Processes chunks obtained with toChunks.
    {-# INLINE processChunks #-}
    processChunks is [] residue =
      return $ S.Skip (is, COResidue_ residue)
    processChunks is (chunk : chunks) residue =
      return $ S.Yield (Right chunk) (is, COProcessChunks_ chunks residue)

    {-# INLINE step' #-}
    -- "is": state of the input stream.
    -- "gst": "global" state? (Inspired by '_compactOnByteCustom' in streamly-0.10.1.)
    step' gst (is, s) = case s of
      COInitOrYieldHeader_ -> do
        istep' <- istep gst is
        case istep' of
          S.Stop -> return S.Stop
          S.Skip is' -> return $ S.Skip (is', COInitOrYieldHeader_)
          S.Yield e is' -> case e of
            Left hdr -> return $ S.Yield (Left hdr) (is', COInitOrYieldHeader_)
            Right newbs -> do
              -- Note: In the initial case (and not just the yield header case), this is possible.
              -- Although a bytestring appearing initially without any preceding header is not what
              -- we have in mind for streamly-archive, we want this function to focus only on the
              -- bytestring splitting.
              let (chunks, residue') = toChunks "" newbs
              return $ S.Skip (is', COProcessChunks_ (toList chunks) residue')
      COResidue_ !residue -> do
        istep' <- istep gst is
        case istep' of
          S.Stop -> return $ S.Yield (Right residue) (is, COStop_)
          S.Skip is' -> return $ S.Skip (is', COResidue_ residue)
          S.Yield e is' -> case e of
            Left hdr -> return $ S.Yield (Right residue) (is', COYieldHeader_ hdr is')
            Right newbs -> do
              let (chunks, residue') = toChunks residue newbs
              return $ S.Skip (is', COProcessChunks_ (toList chunks) residue')
      COStop_ -> return S.Stop
      COYieldHeader_ !hdr !is' -> return $ S.Yield (Left hdr) (is', COInitOrYieldHeader_)
      COProcessChunks_ !chunks !residue ->
        processChunks is chunks residue

-- | The state of the outer 'chunkOnFold' fold.
data ChunkOnFoldState_
  = -- | The initialization of the fold is complete. This state occurs only once (in the beginning).
    Init_
  | -- | The processing of a header is complete.
    Header_
  | -- | The processing of chunks is complete, and a residue (possibly empty) has been made
    -- available.
    Chunks_ !ByteString

-- | Chunks up the bytestrings following each @Left@ by the given word, discarding the given word.
-- (For instance, the word could be @10@ (newline), which gives us lines as the chunks.) The
-- bytestrings in the provided fold are the desired chunks.
{-# INLINE chunkOnFold #-}
chunkOnFold ::
  (Monad m) =>
  Word8 ->
  Fold m (Either a ByteString) b ->
  Fold m (Either a ByteString) b
chunkOnFold splitWd (F.Fold chstep chinit chextr chfinal) =
  -- "ch": chunk.
  let -- A utility function to consume all the chunks available in the same iteration.
      {-# INLINE go #-}
      go chs [] = return $ F.Partial chs -- "chs": state of the chunk fold.
      go chs (chbs : chbss) = do
        chstep' <- chstep chs (Right chbs)
        case chstep' of
          F.Done a -> return $ F.Done a
          F.Partial chs' -> go chs' chbss
      -- A utility function to obtain (chunks, next residue) from the previous residue and the
      -- latest incoming bytestring.
      {-# INLINE toChunks #-}
      toChunks residue newbs =
        -- Non-empty newbs expected.
        let tentativeChunks = Seq.fromList . B.split splitWd $ residue `B.append` newbs
         in case tentativeChunks of
              Seq.Empty -> (Seq.empty, "")
              init' Seq.:|> last' ->
                -- Note: This logic works also when newbs ends with splitWd because then the last
                -- chunk is the empty bytestring.
                (init', last')

      {-# INLINE processHeader #-}
      processHeader chs hdr = do
        chstep' <- chstep chs (Left hdr)
        case chstep' of
          F.Done a -> return $ F.Done a
          F.Partial chs' -> return $ F.Partial (chs', Header_)
      {-# INLINE processBytestring #-}
      processBytestring chs residue chbs = do
        let (chunks, residue') = toChunks residue chbs
        chstep' <- go chs (toList chunks)
        case chstep' of
          F.Done a -> return $ F.Done a
          F.Partial chs' -> return $ F.Partial (chs', Chunks_ residue')
   in -- Note: If a file ends with "\n", we want to include the last empty line.
      F.Fold
        ( \(chs, s) e -> case s of
            Init_ -> case e of
              Left hdr -> do
                processHeader chs hdr
              Right newbs ->
                -- This case is possible. Although a bytestring appearing initially without any
                -- preceding header is not what we have in mind for streamly-archive, we want this
                -- fold to focus only on the bytestring splitting.
                processBytestring chs "" newbs
            Header_ -> case e of
              Left hdr -> do
                -- No bytestrings followed the previous header.
                processHeader chs hdr
              Right newbs ->
                processBytestring chs "" newbs
            Chunks_ residue -> case e of
              Left hdr -> do
                chstep' <- chstep chs (Right residue)
                case chstep' of
                  F.Done a -> return $ F.Done a
                  F.Partial chs' -> do
                    processHeader chs' hdr
              Right newbs ->
                processBytestring chs residue newbs
        )
        ( do
            chstep' <- chinit
            case chstep' of
              F.Done a -> return $ F.Done a
              F.Partial chs' -> return $ F.Partial (chs', Init_)
        )
        (\(chs, _) -> chextr chs)
        ( \(chs, s) -> case s of
            Chunks_ residue -> do
              chstep' <- chstep chs (Right residue)
              case chstep' of
                F.Done a -> return a
                F.Partial chs' -> chfinal chs'
            _ -> chfinal chs
        )
