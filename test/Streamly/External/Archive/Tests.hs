{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Streamly.External.Archive.Tests (tests) where

import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip (compress)
import Control.Monad (forM)
import Crypto.Random.Entropy (getEntropy)
import Data.Bifunctor (first)
import Data.ByteString (ByteString, append)
import qualified Data.ByteString as B
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as LB
import Data.Char (chr, ord)
import Data.Function ((&))
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Archive
import Streamly.External.Archive.Internal.Foreign (blockSize)
import Streamly.Internal.Data.Fold.Type (Fold (Fold), Step (Partial))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (addTrailingPathSeparator, hasTrailingPathSeparator, joinPath, takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.QuickCheck (Gen, choose, frequency, vectorOf)
import Test.QuickCheck.Monadic (monadicIO, pick, run)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)
import Test.Tasty.QuickCheck (testProperty)

tests :: [TestTree]
tests =
  [ testTar False,
    testTar True,
    testSparse
  ]

-- | Use other libraries to create a tar (or tar.gz) file containing random data,
-- read the file back using our library, and check if the results are as expected.
testTar :: Bool -> TestTree
testTar gz = testProperty ("tar (" ++ (if gz then "gz" else "no gz") ++ ")") $ monadicIO $ do
  -- Generate a random file system hierarchy.
  hierarchy <- pick $ randomHierarchy "" 4 4 5

  -- Create a new temporary directory, write our hierarchy into
  -- a "files" subdirectory of the temporary directory, use other
  -- libraries to create files.tar (or files.tar.gz), read the file
  -- back using our library, and check if the results are as expected.
  run . withSystemTempDirectory "archive-streaming-testZip" $ \tmpDir -> do
    let filesDir = joinPath [tmpDir, "files"]
    createDirectoryIfMissing True filesDir
    pathsAndByteStrings <- writeHierarchy filesDir hierarchy

    let archFile = joinPath [tmpDir, "files.tar" ++ (if gz then ".gz" else "")]
    LB.writeFile archFile . (if gz then compress else id) . Tar.write =<< Tar.pack tmpDir ["files"]

    let fileFold =
          Fold
            ( \(mfp, mtyp, msz, mbs) e ->
                case e of
                  Left h -> do
                    mfp_ <- headerPathName h
                    mtyp_ <- headerFileType h
                    msz_ <- headerSize h
                    return $ Partial (unpack <$> mfp_, mtyp_, msz_, mbs)
                  Right bs ->
                    return $
                      Partial
                        ( mfp,
                          mtyp,
                          msz,
                          case mbs of
                            Nothing -> Just bs
                            Just bs' -> Just $ bs' `append` bs
                        )
            )
            (return $ Partial (Nothing, Nothing, Nothing, Nothing))
            return

    pathsFileTypesSizesAndByteStrings <-
      S.unfold (readArchive archFile) undefined
        & groupByHeader fileFold
        & fmap (\(mfp, mtyp, msz, mbs) -> (fromJust mfp, fromJust mtyp, msz, mbs))
        & S.toList

    -- Make the file paths and ByteStrings comparable and compare them.
    let pathsAndByteStrings_ =
          sort $ map (first ("files/" ++)) (("", Nothing) : pathsAndByteStrings)
    let pathAndByteStrings2_ =
          sort . map (\(x, _, _, y) -> (x, y)) $ pathsFileTypesSizesAndByteStrings
    let samePathsAndByteStrings = pathsAndByteStrings_ == pathAndByteStrings2_

    -- Check FileType.
    let fileTypesCorrect =
          all
            ( \(fp, typ, _, _) ->
                if hasTrailingPathSeparator fp
                  then typ == FileTypeDirectory
                  else typ == FileTypeRegular
            )
            pathsFileTypesSizesAndByteStrings

    -- Check header file size.
    let fileSizeCorrect =
          all
            ( \(_, _, msz, mbs) ->
                case (msz, mbs) of
                  (Nothing, _) -> False -- The size is always available.
                  (Just sz, Nothing) -> sz == 0 -- File or directory.
                  (Just sz, Just bs) -> fromIntegral sz == B.length bs
            )
            pathsFileTypesSizesAndByteStrings

    return $ samePathsAndByteStrings && fileTypesCorrect && fileSizeCorrect

-- | Read a fixed sparse file (sparse.tar) using our library and make sure the results are as
-- expected. (The file was created manually on Linux with "cp --sparse=always" to create the sparse
-- files and "tar -Scvf" to create the archive. We were unable to do the equivalent thing on macOS
-- Mojave / APFS.)
testSparse :: TestTree
testSparse = testCase "sparse" $ do
  let fileFold =
        Fold
          ( \(mfp, mbs) e ->
              case e of
                Left h -> do
                  mfp_ <- headerPathName h
                  return $ Partial (unpack <$> mfp_, mbs)
                Right bs ->
                  return $
                    Partial
                      ( mfp,
                        case mbs of
                          Nothing -> Just bs
                          Just bs' -> Just $ bs' `append` bs
                      )
          )
          (return $ Partial (Nothing, Nothing))
          return

  archive <-
    S.unfold (readArchive "test/data/sparse.tar") undefined
      & groupByHeader fileFold
      & fmap (\(mfp, mbs) -> (fromJust mfp, fromJust mbs))
      & S.toList

  assertEqual "" (map fst archive) ["zero", "zeroZero", "zeroAsdf", "asdfZero"]

  let tenMb = 10_000_000
  let zero = B.replicate tenMb 0
  let asdf = "asdf"

  assertBool "unexpected bytestring (1)" $ snd (head archive) == zero
  assertBool "unexpected bytestring (2)" $ snd (archive !! 1) == zero `B.append` zero
  assertBool "unexpected bytestring (3)" $ snd (archive !! 2) == zero `B.append` asdf
  assertBool "unexpected bytestring (4)" $ snd (archive !! 3) == asdf `B.append` zero

-- | Writes a given hierarchy of relative paths (created with 'randomHierarchy') to disk
-- in the specified directory and returns the same hierarchy except with actual ByteStrings
-- instead of lengths. Note: The original relative paths are returned back unaltered.
writeHierarchy :: FilePath -> [(FilePath, Maybe Int)] -> IO [(FilePath, Maybe ByteString)]
writeHierarchy writeDir = mapM $ \(p, mBsLen) ->
  let fullp = joinPath [writeDir, p]
   in case mBsLen of
        Just bsLen -> do
          createDirectoryIfMissing True (takeDirectory fullp)
          bs <- getEntropy (fromIntegral bsLen)
          B.writeFile fullp bs
          return
            ( p,
              if bsLen == 0
                then Nothing -- Our library yields no ByteString at all for empty files.
                else Just bs
            )
        Nothing -> createDirectoryIfMissing True fullp >> return (p, Nothing)

-- | Recursively generates a random hierarchy of relative paths to files and
-- directories. (Nothing is written to disk; only the paths are returned.)
-- The initial dirPath should be "". A random bytestring length is
-- provided in case of a file; 'Nothing' in the case of a directory.
randomHierarchy :: FilePath -> Int -> Int -> Int -> Gen [(FilePath, Maybe Int)]
randomHierarchy dirPath maxFiles maxDirs maxDepth = do
  numFiles <- choose (0, maxFiles)
  fileComps <- nub <$> vectorOf numFiles pathComponent
  let filePaths = map (\c -> joinPath [dirPath, c]) fileComps
  bsLengths <-
    map Just
      <$> vectorOf
        (length filePaths)
        ( frequency
            [ (1, choose (0, 5)),
              (1, choose (blockSize - 5, blockSize + 5)),
              (1, choose (0, 3 * blockSize))
            ]
        )

  numDirs <- choose (0, maxDirs)
  dirComps <-
    nub . filter (not . (`elem` fileComps))
      <$> vectorOf (if maxDepth <= 0 then 0 else numDirs) pathComponent
  -- libarchive reads back directory paths with a trailing separator.
  let dirPaths = map (\c -> addTrailingPathSeparator $ joinPath [dirPath, c]) dirComps

  recursion <-
    concat
      <$> forM
        dirPaths
        ( \dirPath' ->
            randomHierarchy dirPath' (maxFiles `div` 2) (maxDirs `div` 2) (maxDepth - 1)
        )

  return $ zip filePaths bsLengths ++ zip dirPaths (repeat Nothing) ++ recursion

-- | Generates a random path component of length between 1 and 10, e.g., "HO53UVKQ".
-- For compatibility with case-insensitive file systems, uses only one case.
pathComponent :: Gen String
pathComponent = do
  len <- choose (1, 10)
  vectorOf len $
    chr
      <$> frequency
        [ (1, choose (ord 'A', ord 'Z')),
          (1, choose (ord '0', ord '9'))
        ]
