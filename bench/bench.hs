{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (forM, forM_)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Vector as V
import Statistics.Sample (mean, stdDev)
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
  shells "cabal build >> $logfile 2>&1" empty

  let c_executable = "./bench-archive"
  hs_plain_executable <-
    T.strip <$> strict (inshell "cabal exec -- which bench-archive-plain 2> /dev/null" empty)
  hs_streamly_executable <-
    T.strip <$> strict (inshell "cabal exec -- which bench-archive-streamly 2> /dev/null" empty)

  let fileCountsAndFileSizes :: [(Int, Int)] =
        [(2500000, 1), (5000000, 1), (20, 250000000), (40, 250000000), (80, 250000000)]

  echoHrule
  answerIsYes "Create archives?"
    >>= flip
      when
      ( do
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
      )

  echoHrule
  answerIsYes "Measure read performance?"
    >>= flip
      when
      ( do
          let tmpDir :: (IsString a) => a
              tmpDir = "./tmp"
          mktree tmpDir

          let csvFile :: (IsString a) => a
              csvFile = "./tmp/read.csv"
          shells (format ("rm -f " % fp % "") csvFile) empty

          output csvFile . return . unsafeTextToLine . T.pack $
            "fileCount,fileSize,"
              ++ "c_mean,c_std,"
              ++ "hsPlain_mean,hsPlain_std,"
              ++ "hsStreamly_mean,hsStreamly_std,"
              ++ "hsPlain_div_c_mean,hsPlain_div_c_std,"
              ++ "hsStreamly_div_c_mean,hsStreamly_div_c_std,"
              ++ "hsStreamly_div_hsPlain_mean,hsStreamly_div_hsPlain_std"

          forM_ fileCountsAndFileSizes $ \(fileCount, fileSize') -> do
            let archivePath =
                  format ("" % fp % "/test_" % d % "_" % d % ".tar") tmpDir fileCount fileSize'
            let args = format ("read " % s % "") archivePath
            let expectedInOutput1 = format ("Total filesize: " % d % "") (fileSize' * fileCount)
            let expectedInOutput2 = format ("File count:     " % d % "") fileCount
            let warmCount = 1
            let timeCount = 10
            procs "echo" [format ("    Reading archive " % s % " with C...") archivePath] empty
            cTimes <-
              timeCommand
                Nothing
                (format ("" % s % " " % s % "") c_executable args)
                [expectedInOutput1, expectedInOutput2]
                warmCount
                timeCount
            procs
              "echo"
              [format ("    Reading archive " % s % " with Haskell (plain)...") archivePath]
              empty
            hsPlainTimes <-
              timeCommand
                Nothing
                (format ("" % s % " " % s % "") hs_plain_executable args)
                [expectedInOutput1, expectedInOutput2]
                warmCount
                timeCount
            procs
              "echo"
              [format ("    Reading archive " % s % " with Haskell (streamly)...") archivePath]
              empty
            hsStreamlyTimes <-
              timeCommand
                Nothing
                (format ("" % s % " " % s % "") hs_streamly_executable args)
                [expectedInOutput1, expectedInOutput2]
                warmCount
                timeCount

            let hsPlainDivCTimes = [a / b | a <- hsPlainTimes, b <- cTimes]
            let hsStreamlyDivCTimes = [a / b | a <- hsStreamlyTimes, b <- cTimes]
            let hsStreamlyDivHsPlainTimes = [a / b | a <- hsStreamlyTimes, b <- hsPlainTimes]

            let mean' :: [NominalDiffTime] -> Double
                mean' xs = mean . V.fromList $ map realToFrac xs

            let std' :: [NominalDiffTime] -> Double
                std' xs = stdDev . V.fromList $ map realToFrac xs

            let line =
                  intercalate "," $
                    [show fileCount, show fileSize']
                      ++ map
                        show
                        [ mean' cTimes,
                          std' cTimes,
                          mean' hsPlainTimes,
                          std' hsPlainTimes,
                          mean' hsStreamlyTimes,
                          std' hsStreamlyTimes,
                          mean' hsPlainDivCTimes,
                          std' hsPlainDivCTimes,
                          mean' hsStreamlyDivCTimes,
                          std' hsStreamlyDivCTimes,
                          mean' hsStreamlyDivHsPlainTimes,
                          std' hsStreamlyDivHsPlainTimes
                        ]
            append csvFile . return . unsafeTextToLine . T.pack $ line
            procs "echo" [format ("    Appended results to " % s % "") csvFile] empty
            return ()
      )

failWithMsg :: Line -> IO a
failWithMsg msg = echo msg >> exit (ExitFailure 1)

echoHrule :: IO ()
echoHrule = echo "----------------------------------------"

answerIsYes :: String -> IO Bool
answerIsYes question = do
  procs "echo" ["-n", T.pack $ question ++ " (y/N) "] empty
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
