module Streamly.External.Archive.Internal.Foreign
  ( Archive,
    Entry,
    FileType (..),
    archive_read_new,
    archive_read_support_filter_all,
    archive_read_support_format_all,
    archive_read_support_format_gnutar,
    blockSize,
    archive_read_open_filename,
    archive_read_next_header,
    archive_entry_filetype,
    archive_entry_pathname,
    archive_entry_pathname_utf8,
    archive_entry_size,
    alloc_archive_read_data_buffer,
    archive_read_data,
    archive_read_data_block,
    archive_read_free,
  )
where

import Control.Exception (Exception, mask_, throw)
import Control.Monad (when)
import Data.Bits ((.&.))
import Data.ByteString (ByteString, packCString, packCStringLen)
import qualified Data.ByteString as B
import Data.Int (Int64)
import Foreign (FunPtr, Ptr, nullPtr, peek)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CChar, CInt (CInt), CSize (CSize))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes)
import System.Posix.Types (CMode (CMode), CSsize (CSsize))

data CArchive

data CEntry

foreign import ccall unsafe "archive.h archive_errno"
  c_archive_errno :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive.h archive_error_string"
  c_archive_error_string :: Ptr CArchive -> IO CString

foreign import ccall unsafe "archive.h archive_read_new"
  c_archive_read_new :: IO (Ptr CArchive)

foreign import ccall unsafe "archive.h archive_read_support_filter_all"
  c_archive_read_support_filter_all :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive.h archive_read_support_format_all"
  c_archive_read_support_format_all :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive.h archive_read_support_format_gnutar"
  c_archive_read_support_format_gnutar :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive.h archive_read_open_filename"
  c_archive_read_open_filename :: Ptr CArchive -> CString -> CSize -> IO CInt

foreign import ccall unsafe "archive.h archive_read_next_header2"
  c_archive_read_next_header2 :: Ptr CArchive -> Ptr CEntry -> IO CInt

foreign import ccall unsafe "archive.h archive_read_data"
  -- Todo: Think about la_ssize_t on non-POSIX.
  c_archive_read_data :: Ptr CArchive -> Ptr CChar -> CSize -> IO CSsize

foreign import ccall unsafe "archive.h archive_read_data_block"
  c_archive_read_data_block :: Ptr CArchive -> Ptr (Ptr CChar) -> Ptr CSize -> Ptr Int64 -> IO CInt

foreign import ccall unsafe "archive.h archive_read_free"
  c_archive_read_free :: Ptr CArchive -> IO CInt

foreign import ccall unsafe "archive_entry.h archive_entry_filetype"
  c_archive_entry_filetype :: Ptr CEntry -> IO CMode -- Todo: Think about type on non-POSIX.

foreign import ccall unsafe "archive_entry.h archive_entry_new"
  c_archive_entry_new :: IO (Ptr CEntry)

-- Similar to c_free_finalizer from ByteString.
foreign import ccall unsafe "static archive_entry.h &archive_entry_free"
  c_archive_entry_free_finalizer :: FunPtr (Ptr CEntry -> IO ())

foreign import ccall unsafe "archive_entry.h archive_entry_pathname"
  c_archive_entry_pathname :: Ptr CEntry -> IO CString

foreign import ccall unsafe "archive_entry.h archive_entry_pathname_utf8"
  c_archive_entry_pathname_utf8 :: Ptr CEntry -> IO CString

foreign import ccall unsafe "archive_entry.h archive_entry_size"
  c_archive_entry_size :: Ptr CEntry -> IO Int64

foreign import ccall unsafe "archive_entry.h archive_entry_size_is_set"
  c_archive_entry_size_is_set :: Ptr CEntry -> IO CInt

-- Documented libarchive return codes.
data RetCode
  = RetCodeEOF
  | RetCodeOK
  | RetCodeRETRY
  | RetCodeWARN
  | RetCodeFAILED
  | RetCodeFATAL
  deriving (Show)

retCodes :: [(CInt, RetCode)]
retCodes =
  [ (1, RetCodeEOF),
    (0, RetCodeOK),
    (-10, RetCodeRETRY),
    (-20, RetCodeWARN),
    (-25, RetCodeFAILED),
    (-30, RetCodeFATAL)
  ]

data ArchiveError = ArchiveError
  { err_function :: !String,
    err_retcode :: !(Either CInt RetCode),
    err_number :: !Int,
    err_string :: !String
  }
  deriving (Show)

instance Exception ArchiveError

newtype ErrorString = ErrorString String deriving (Show)

instance Exception ErrorString

archive_error_string :: Ptr CArchive -> IO String
archive_error_string aptr = do
  cstr <- c_archive_error_string aptr
  if cstr == nullPtr
    then return "archive_error_string returned NULL"
    else peekCString cstr

throwArchiveError :: String -> CInt -> Ptr CArchive -> IO noReturn
throwArchiveError fn rc aptr = do
  num <- fromIntegral <$> c_archive_errno aptr
  str <- archive_error_string aptr
  throw $
    ArchiveError
      { err_function = fn,
        err_retcode = maybe (Left rc) Right (lookup rc retCodes),
        err_number = num,
        err_string = str
      }

newtype Archive = Archive (Ptr CArchive)

newtype Entry = Entry (ForeignPtr CEntry)

data FileType
  = FileTypeRegular
  | FileTypeSymlink
  | FileTypeSocket
  | FileTypeCharDevice
  | FileTypeBlockDevice
  | FileTypeDirectory
  | FileTypeNamedPipe
  deriving (Show, Eq)

archive_read_new :: IO Archive
archive_read_new = do
  aptr <- c_archive_read_new
  if aptr == nullPtr
    then throw $ ErrorString "archive_read_new returned NULL"
    else return $ Archive aptr

archive_read_support_filter_all :: Archive -> IO ()
archive_read_support_filter_all (Archive aptr) = do
  rc <- c_archive_read_support_filter_all aptr
  when (rc /= 0) $ throwArchiveError "archive_read_support_filter_all" rc aptr

archive_read_support_format_all :: Archive -> IO ()
archive_read_support_format_all (Archive aptr) = do
  rc <- c_archive_read_support_format_all aptr
  when (rc /= 0) $ throwArchiveError "archive_read_support_format_all" rc aptr

archive_read_support_format_gnutar :: Archive -> IO ()
archive_read_support_format_gnutar (Archive aptr) = do
  rc <- c_archive_read_support_format_gnutar aptr
  when (rc /= 0) $ throwArchiveError "archive_read_support_format_gnutar" rc aptr

-- Fixed block size for now.
{-# INLINE blockSize #-}
blockSize :: (Num a) => a
blockSize = 4096

archive_read_open_filename :: Archive -> FilePath -> IO ()
archive_read_open_filename (Archive aptr) fp =
  withCString fp $ \cstr -> do
    rc <- c_archive_read_open_filename aptr cstr blockSize
    when (rc /= 0) $ throwArchiveError "archive_read_open_filename" rc aptr

-- | Returns 'Nothing' if we have reached the end of the archive.
{-# INLINE archive_read_next_header #-}
archive_read_next_header :: Archive -> IO (Maybe Entry)
archive_read_next_header (Archive aptr) = do
  fpe <- mask_ $ c_archive_entry_new >>= newForeignPtr c_archive_entry_free_finalizer
  rc <- withForeignPtr fpe $ c_archive_read_next_header2 aptr
  if rc == 1 -- EOF.
    then return Nothing
    else
      if rc < 0
        then throwArchiveError "archive_read_next_header" rc aptr
        else return . Just . Entry $ fpe

{-# INLINE fileTypeAeIFMT #-}
fileTypeAeIFMT :: CMode
fileTypeAeIFMT = 0o0170000

{-# INLINE fileTypes #-}
fileTypes :: [(CMode, FileType)]
fileTypes =
  [ (0o0100000, FileTypeRegular),
    (0o0120000, FileTypeSymlink),
    (0o0140000, FileTypeSocket),
    (0o0020000, FileTypeCharDevice),
    (0o0060000, FileTypeBlockDevice),
    (0o0040000, FileTypeDirectory),
    (0o0010000, FileTypeNamedPipe)
  ]

{-# INLINE archive_entry_filetype #-}
archive_entry_filetype :: Entry -> IO (Maybe FileType)
archive_entry_filetype (Entry feptr) = withForeignPtr feptr $ \eptr -> do
  i <- c_archive_entry_filetype eptr
  return $ lookup (i .&. fileTypeAeIFMT) fileTypes

{-# INLINE archive_entry_pathname #-}
archive_entry_pathname :: Entry -> IO (Maybe ByteString)
archive_entry_pathname (Entry feptr) = withForeignPtr feptr $ \eptr -> do
  cstr <- c_archive_entry_pathname eptr
  if cstr == nullPtr
    then return Nothing
    else Just <$> packCString cstr

{-# INLINE archive_entry_pathname_utf8 #-}
archive_entry_pathname_utf8 :: Entry -> IO (Maybe ByteString)
archive_entry_pathname_utf8 (Entry feptr) = withForeignPtr feptr $ \eptr -> do
  cstr <- c_archive_entry_pathname_utf8 eptr
  if cstr == nullPtr
    then return Nothing
    else Just <$> packCString cstr

{-# INLINE archive_entry_size #-}
archive_entry_size :: Entry -> IO (Maybe Int)
archive_entry_size (Entry feptr) = withForeignPtr feptr $ \eptr -> do
  size_is_set <- (/= 0) <$> c_archive_entry_size_is_set eptr
  if size_is_set
    then Just . fromIntegral <$> c_archive_entry_size eptr
    else return Nothing

-- | Please free after use.
alloc_archive_read_data_buffer :: IO (Ptr CChar)
alloc_archive_read_data_buffer = mallocBytes blockSize

-- | Returns 'Nothing' if there is no more data for the current entry.
-- Pass in a buffer allocated with 'alloc_archive_read_data_buffer'.
{-# INLINE archive_read_data #-}
archive_read_data :: Archive -> Ptr CChar -> IO (Maybe ByteString)
archive_read_data (Archive aptr) buf = do
  rb <- c_archive_read_data aptr buf blockSize
  if rb == 0
    then return Nothing
    else
      if rb < 0
        then throwArchiveError "archive_read_data" (fromIntegral rb) aptr
        else Just <$> packCStringLen (buf, fromIntegral rb)

{-# INLINE archive_read_data_block #-}
archive_read_data_block ::
  Archive ->
  Ptr (Ptr CChar) ->
  Ptr CSize ->
  Ptr Int64 ->
  Int64 ->
  IO (ByteString, Bool)
archive_read_data_block (Archive aptr) buf sz offs pos = do
  rc <- c_archive_read_data_block aptr buf sz offs
  if rc < 0
    then throwArchiveError "archive_read_data_block" (fromIntegral rc) aptr
    else
      if rc == 0 || rc == 1
        then do
          -- OK or EOF.
          bs <- peek buf >>= \buf' -> peek sz >>= \sz' -> packCStringLen (buf', fromIntegral sz')
          offs' <- peek offs
          -- pos: Where we are currently located and where the data goes normally (for non-sparse
          -- files). offs': Where libarchive is asking us to position the data.
          if offs' == pos
            then return (bs, rc == 1)
            else
              if offs' > pos
                then do
                  -- For a sparse file, we need to prepend zeroes to the normal data.
                  let diff = offs' - pos
                  let bs' = B.replicate (fromIntegral diff) 0 `B.append` bs
                  return (bs', rc == 1)
                else throw $ ErrorString "archive_read_data_block: unexpected offset"
        else throw $ ErrorString "archive_read_data_block: unexpected return code"

archive_read_free :: Archive -> IO ()
archive_read_free (Archive aptr) = do
  rc <- c_archive_read_free aptr
  when (rc /= 0) $ throwArchiveError "archive_read_free" rc aptr
