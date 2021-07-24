{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Data.Function((&))
import Data.Maybe (fromJust, isJust)
import Streamly.External.Archive (headerSize, readArchive)
import System.Environment (getArgs)

import qualified Data.ByteString as B
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Prelude as S

main :: IO Int
main = getArgs >>= dispatch

dispatch :: [String] -> IO Int
dispatch ["read", path] = do
    let fol = FL.mkFoldM
            (\(!lastHeaderSize, !fileSize, !totalFilesize, !numFiles) e ->
                case e of
                    Left h ->
                        if isJust lastHeaderSize && (fromJust lastHeaderSize < 0 || fromJust lastHeaderSize /= fileSize) then
                            error "unexpected file size in archive"
                        else do
                            hsz <- headerSize h
                            return $ FL.Partial (hsz, 0, totalFilesize + fromJust hsz, numFiles + 1)
                    Right d ->
                        return $ FL.Partial (lastHeaderSize, fileSize + fromIntegral (B.length d), totalFilesize, numFiles))
            (return $ FL.Partial (Nothing, 0, 0, 0 :: Int))
            (\(_, _, x, y) -> return (x, y))

    (totalFileSize, fileCount) <-
        S.unfold (readArchive path) undefined
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
