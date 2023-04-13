{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as B
import Data.Int (Int64)
import Data.Maybe (fromJust)
import Foreign (malloc)
import Streamly.External.Archive.Internal.Foreign
  ( archive_entry_size,
    archive_read_data_block,
    archive_read_free,
    archive_read_new,
    archive_read_next_header,
    archive_read_open_filename,
    archive_read_support_filter_all,
    archive_read_support_format_all,
  )
import System.Environment (getArgs)

main :: IO Int
main = getArgs >>= dispatch

dispatch :: [String] -> IO Int
dispatch ["read", path] = do
  arch <- archive_read_new
  archive_read_support_filter_all arch
  archive_read_support_format_all arch
  archive_read_open_filename arch path

  buf <- malloc
  sz <- malloc
  offs <- malloc

  let go :: Int -> Int -> IO (Int, Int)
      go !totalFilesize !numFiles = do
        pe <- archive_read_next_header arch
        case pe of
          Nothing -> return (totalFilesize, numFiles)
          Just e -> do
            entrySz <- archive_entry_size e

            let go2 :: Int64 -> IO Int64
                go2 !pos = do
                  (b, done) <- archive_read_data_block arch buf sz offs pos
                  if done
                    then return $ pos + fromIntegral (B.length b)
                    else go2 $ pos + fromIntegral (B.length b)

            readSz <- go2 0

            if fromJust entrySz < 0 || fromJust entrySz /= fromIntegral readSz
              then error "unexpected file size in archive"
              else go (totalFilesize + fromIntegral (fromJust entrySz)) (numFiles + 1)

  (totalFilesize, numFiles) <- go 0 0
  archive_read_free arch

  putStrLn $ "Total filesize: " ++ show totalFilesize
  putStrLn $ "File count:     " ++ show numFiles

  return 0
dispatch _ = do
  printUsage
  return 1

printUsage :: IO ()
printUsage =
  putStrLn "bench-archive read [path]"
