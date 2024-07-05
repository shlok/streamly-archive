{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.ByteString as B
import Data.Function
import Data.Maybe
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Archive
import qualified Streamly.Internal.Data.Fold as F
import System.Environment

main :: IO Int
main = getArgs >>= dispatch

dispatch :: [String] -> IO Int
dispatch ["read", path] = do
  let fol =
        F.foldtM'
          ( \(!lastHeaderSize, !fileSize, !totalFilesize, !numFiles) e ->
              case e of
                Left h ->
                  if isJust lastHeaderSize
                    && (fromJust lastHeaderSize < 0 || fromJust lastHeaderSize /= fileSize)
                    then error "unexpected file size in archive"
                    else do
                      hsz <- headerSize h
                      return $ F.Partial (hsz, 0, totalFilesize + fromJust hsz, numFiles + 1)
                Right d ->
                  return $
                    F.Partial
                      ( lastHeaderSize,
                        fileSize + fromIntegral (B.length d),
                        totalFilesize,
                        numFiles
                      )
          )
          (return $ F.Partial (Nothing, 0, 0, 0 :: Int))
          (\(_, _, x, y) -> return (x, y))

  (totalFileSize, fileCount) <-
    S.unfold readArchive path
      & S.fold fol

  putStrLn $ "Total filesize: " ++ show totalFileSize
  putStrLn $ "File count:     " ++ show fileCount

  return 0
dispatch _ = do
  printUsage
  return 1

printUsage :: IO ()
printUsage =
  putStrLn "bench-archive read [path]"
