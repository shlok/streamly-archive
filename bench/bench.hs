{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.List
import qualified Data.Text as T
import Data.Time.Clock
import qualified Data.Vector as V
import Statistics.Sample
import Turtle

data Platform = PlatformLinux | PlatformMacOS deriving (Eq)

main :: IO ()
main = do
  uname <- strict $ inshell "uname" empty
  platform <-
    if "Linux" `T.isInfixOf` uname
      then return PlatformLinux
      else
        if "Darwin" `T.isInfixOf` uname
          then return PlatformMacOS
          else failWithMsg "Currently only Linux and macOS are supported."

  -- Required for pcregrep on Linux.
  when (platform == PlatformLinux) $ export "LC_ALL" "en_US.UTF-8"

  let logFile :: (IsString a) => a
      logFile = "./tmp/bench.sh.log"

  export "logfile" (fromString logFile)
  testfile logFile >>= flip when (rm logFile)

  echo "Compiling C program... "
  shells "mkdir -p ./tmp" empty
  shells
    ( "gcc -O2 -Wall bench-archive.c -I/usr/local/opt/libarchive/include "
        `T.append` "-L/usr/local/opt/libarchive/lib "
        `T.append` "-larchive -o bench-archive >> $logfile 2>&1"
    )
    empty

  echo "Compiling Haskell programs... "
  shells "cabal build -ffusion-plugin >> $logfile 2>&1" empty

  let c_executable = "./bench-archive"

  -- Verbosity 0 helped avoid the beginning "Resolving dependencies..." output.
  hs_plain_executable <-
    T.strip
      <$> strict (inshell "cabal exec --verbose=0 -- which bench-archive-plain 2> /dev/null" empty)
  hs_streamly_executable <-
    T.strip
      <$> strict
        (inshell "cabal exec --verbose=0 -- which bench-archive-streamly 2> /dev/null" empty)

  -- The files will be smaller than our RAM (64 GB). Due to warmCount/timeCount below, these files
  -- might get memory mapped by the OS. This seems fine because we’re comparing our Haskell and C
  -- programs when they can run as fast as possible. (For files larger than RAM, we might begin to
  -- measure irrelevant things we can’t improve on.)
  let fileCountsAndFileSizes :: [(Int, Int)] =
        [ -- 1 byte files. (It turned out each file uses up at least ~1 KB in the archive.)
          (2_500_000, 1),
          (5_000_000, 1),
          (10_000_000, 1), -- ~10 GB.
          -- 100 byte files.
          (2_500_000, 100),
          (5_000_000, 100),
          (10_000_000, 100), -- ~10 GB.
          -- 1 KB files.
          (2_500_000, 1000),
          (5_000_000, 1000),
          (10_000_000, 1000), -- ~10 GB.
          -- 10 KB files.
          (250_000, 10_000),
          (500_000, 10_000),
          (1_000_000, 10_000), -- ~10 GB.
          -- 100 KB files.
          (25_000, 100_000),
          (50_000, 100_000),
          (100_000, 100_000), -- ~10 GB.
          -- 1 MB files.
          (2_500, 1_000_000),
          (5_000, 1_000_000),
          (10_000, 1_000_000), -- ~10 GB.
          -- 10 MB files.
          (250, 10_000_000),
          (500, 10_000_000),
          (1000, 10_000_000) -- ~10 GB.
        ]

  autoYes <- answerIsYes False "Run all benchmarks without asking?"

  echoHrule
  answerIsYes autoYes "Create archives?"
    >>= flip
      when
      ( do
          t1 <- getCurrentTime
          echo "Creating archives using C program (unless they already exist)..."
          let tmpDir :: (IsString a) => a
              tmpDir = "./tmp"
          mktree tmpDir

          forM_ fileCountsAndFileSizes $ \(fileCount, fileSize') -> do
            let archivePath =
                  format ("" % fp % "/test_" % d % "_" % d % ".tar") tmpDir fileCount fileSize'
            (_, existingFileCount) <-
              shellStrict (format ("tar -tf " % s % " 2> /dev/null | wc -l | bc") archivePath) empty
            if T.strip existingFileCount == format ("" % d % "") fileCount
              then procs "echo" [format ("    Archive " % s % " already exists.") archivePath] empty
              else do
                procs "echo" ["-n", format ("    Creating archive " % s % "... ") archivePath] empty
                shells (format ("rm -rf " % s % " 2> /dev/null") archivePath) empty
                shells
                  ( format
                      ("" % s % " write " % s % " " % d % " " % d % "")
                      c_executable
                      archivePath
                      fileCount
                      fileSize'
                  )
                  empty
                echo "success."
          t2 <- getCurrentTime
          printf
            ("Time taken for creating archives: " % f % " hours.\n")
            (realToFrac (diffUTCTime t2 t1) / (60 * 60))
      )

  echoHrule
  answerIsYes autoYes "Measure read performance?"
    >>= flip
      when
      ( do
          t1 <- getCurrentTime
          let tmpDir :: (IsString a) => a
              tmpDir = "./tmp"
          mktree tmpDir

          let csvFile :: (IsString a) => a
              csvFile = "./tmp/read.csv"
          shells (format ("rm -f " % fp % "") csvFile) empty

          output csvFile . return . unsafeTextToLine . T.pack $
            "fileCount,fileSize,totalBytes,"
              ++ "c_mean,c_std,"
              ++ "hsPlain_mean,hsPlain_std,"
              ++ "hsStreamly_mean,hsStreamly_std"

          forM_ fileCountsAndFileSizes $ \(fileCount, fileSize') -> do
            let totalBytes = fileCount * fileSize'
            let archivePath =
                  format ("" % fp % "/test_" % d % "_" % d % ".tar") tmpDir fileCount fileSize'
            let args = format ("read " % s % "") archivePath
            let expectedInOutput1 = format ("Total filesize: " % d % "") (fileSize' * fileCount)
            let expectedInOutput2 = format ("File count:     " % d % "") fileCount
            let warmCount = 1
            let timeCount = 10
            procs "echo" [format ("    Reading archive " % s % " with C...") archivePath] empty
            (cMean, cStd) <-
              toStats totalBytes
                <$> timeCommand
                  Nothing
                  (format ("" % s % " " % s % "") c_executable args)
                  [expectedInOutput1, expectedInOutput2]
                  warmCount
                  timeCount
            procs
              "echo"
              [format ("    Reading archive " % s % " with Haskell (plain)...") archivePath]
              empty
            (hsPlainMean, hsPlainStd) <-
              toStats totalBytes
                <$> timeCommand
                  Nothing
                  (format ("" % s % " " % s % "") hs_plain_executable args)
                  [expectedInOutput1, expectedInOutput2]
                  warmCount
                  timeCount
            procs
              "echo"
              [format ("    Reading archive " % s % " with Haskell (streamly)...") archivePath]
              empty
            (hsStreamlyMean, hsStreamlyStd) <-
              toStats totalBytes
                <$> timeCommand
                  Nothing
                  (format ("" % s % " " % s % "") hs_streamly_executable args)
                  [expectedInOutput1, expectedInOutput2]
                  warmCount
                  timeCount

            let line =
                  intercalate "," $
                    [show fileCount, show fileSize', show totalBytes]
                      ++ map
                        show
                        [ cMean,
                          cStd,
                          hsPlainMean,
                          hsPlainStd,
                          hsStreamlyMean,
                          hsStreamlyStd
                        ]

            append csvFile . return . unsafeTextToLine . T.pack $ line
            procs "echo" [format ("    Appended results to " % s % "") csvFile] empty

          t2 <- getCurrentTime
          printf
            ("Time taken for measuring read performance: " % f % " hours.\n")
            (realToFrac (diffUTCTime t2 t1) / (60 * 60))
      )

failWithMsg :: Line -> IO a
failWithMsg msg = echo msg >> exit (ExitFailure 1)

echoHrule :: IO ()
echoHrule = echo "----------------------------------------"

answerIsYes :: Bool -> String -> IO Bool
answerIsYes autoYes question = do
  procs "echo" ["-n", T.pack $ question ++ " (y/N) "] empty
  if autoYes
    then do
      procs "echo" ["(automatic yes)"] empty
      return True
    else do
      line <- readline
      case line of
        Just answer | answer == "y" -> return True
        _ -> return False

timeCommand' :: Text -> [Text] -> IO NominalDiffTime
timeCommand' cmdAndArgs expectedInOut = do
  (out, t) <- time . strict $ inshell cmdAndArgs empty
  if all (`T.isInfixOf` out) expectedInOut
    then return t
    else
      error $
        "all of "
          ++ show expectedInOut
          ++ " were not found in output "
          ++ show out
          ++ " of command "
          ++ show cmdAndArgs

timeCommand :: Maybe T.Text -> Text -> [Text] -> Int -> Int -> IO [NominalDiffTime]
timeCommand forceRemovalPath cmdAndArgs expectedInOut warmCount timeCount = do
  let removePath =
        maybe (return ()) (\fp' -> shells (format ("rm -rf " % s % "") fp') empty) forceRemovalPath
  let echoCount c = procs "echo" ["-n", format ("" % d % " ") c] empty
  when (warmCount > 0) $ do
    procs "echo" ["-n", "        Warming up... "] empty
    forM_ [1 .. warmCount] $ \c ->
      echoCount c >> removePath >> timeCommand' cmdAndArgs expectedInOut
    echo "done."
  procs "echo" ["-n", "        Timing... "] empty
  times <- forM [1 .. timeCount] $ \c ->
    echoCount c >> removePath >> timeCommand' cmdAndArgs expectedInOut
  removePath
  echo "done."
  return times

-- Mean and standard deviation of nanoseconds per byte.
toStats :: Int -> [NominalDiffTime] -> (Double, Double)
toStats bytes' secs =
  let nsPerByte =
        V.fromList $ map (realToFrac . (/ fromIntegral bytes') . (* 1e9)) secs
   in (mean nsPerByte, stdDev nsPerByte)
