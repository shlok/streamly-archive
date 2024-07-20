{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Streamly.External.Archive.Tests (tests) where

import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip
import Control.Concurrent.Async
import Control.Monad
import Crypto.Random.Entropy
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as LB
import Data.Char
import Data.Function
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Set as Set
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Archive
import Streamly.External.Archive.Internal.Foreign
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: [TestTree]
tests =
  [ testTar False,
    testTar True,
    testSparse
  ]

-- | Use other libraries to create a tar (or tar.gz) file containing random data, read the file back
-- using our library, and check if the results are as expected.
testTar :: Bool -> TestTree
testTar gz = testProperty ("tar (" ++ (if gz then "gz" else "no gz") ++ ")") $ monadicIO $ do
  -- Generate a random file system hierarchy for writing to disk.
  hierarchyToWrite <- pick $ randomHierarchy "" 4 4 5

  numThreads <- pick $ chooseInt (1, 4)

  -- Of the hierarchy we wrote, sometimes we only read some of them back.
  readSome :: Bool <- pick arbitrary
  readSomePaths <-
    pick $
      sublistOf hierarchyToWrite
        <&> Set.fromList
          . map (("files/" ++) . fst) -- Make comparable to what our library reads back.

  -- Create a new temporary directory, write our hierarchy into a "files" subdirectory of the
  -- temporary directory, use other libraries to create files.tar (or files.tar.gz), read the file
  -- back using our library, and check if the results are as expected.
  run . withSystemTempDirectory "archive-streaming-testZip" $ \tmpDir -> do
    let filesDir = joinPath [tmpDir, "files"]
    createDirectoryIfMissing True filesDir
    writePathsAndByteStrings <- writeHierarchy filesDir hierarchyToWrite

    let archFile = joinPath [tmpDir, "files.tar" ++ (if gz then ".gz" else "")]
    LB.writeFile archFile . (if gz then compress else id) . Tar.write =<< Tar.pack tmpDir ["files"]

    let fileFold =
          F.foldlM'
            ( \(mfp, mtyp, msz, mbs) e ->
                case e of
                  Left h -> do
                    mfp_ <- headerPathName h
                    mtyp_ <- headerFileType h
                    msz_ <- headerSize h
                    return (BC.unpack <$> mfp_, mtyp_, msz_, mbs)
                  Right bs ->
                    return
                      ( mfp,
                        mtyp,
                        msz,
                        case mbs of
                          Nothing -> Just bs
                          Just bs' -> Just $ bs' `BC.append` bs
                      )
            )
            (return (Nothing, Nothing, Nothing, Nothing))

    readPathsFileTypesSizesAndByteStringss <-
      replicateConcurrently numThreads $
        S.unfold
          readArchive
          ( if readSome
              then
                mapHeaderMaybe
                  ( \h -> do
                      p <- fromJust <$> headerPathName h
                      return $
                        if BC.unpack p `Set.member` readSomePaths
                          then Just h
                          else Nothing
                  )
              else
                id,
            archFile
          )
          & groupByHeader fileFold
          & fmap (\(mfp, mtyp, msz, mbs) -> (fromJust mfp, fromJust mtyp, msz, mbs))
          & S.toList

    threadResults <- forM readPathsFileTypesSizesAndByteStringss $
      \readPathsFileTypesSizesAndByteStrings -> do
        let readPathAndByteStrings =
              sort . map (\(x, _, _, y) -> (x, y)) $ readPathsFileTypesSizesAndByteStrings

        let writePathsAndByteStrings2 =
              sort
                . (if readSome then filter (\(x, _) -> x `Set.member` readSomePaths) else id)
                . map (first ("files/" ++)) -- Make comparable to what our library reads back.
                $ ("", Nothing) : writePathsAndByteStrings

        let samePathsAndByteStrings = writePathsAndByteStrings2 == readPathAndByteStrings

        -- Check FileType.
        let fileTypesCorrect =
              all
                ( \(fp, typ, _, _) ->
                    if hasTrailingPathSeparator fp
                      then typ == FileTypeDirectory
                      else typ == FileTypeRegular
                )
                readPathsFileTypesSizesAndByteStrings

        -- Check header file size.
        let fileSizeCorrect =
              all
                ( \(_, _, msz, mbs) ->
                    case (msz, mbs) of
                      (Nothing, _) -> False -- The size is always available.
                      (Just sz, Nothing) -> sz == 0 -- File or directory.
                      (Just sz, Just bs) -> fromIntegral sz == B.length bs
                )
                readPathsFileTypesSizesAndByteStrings

        return $ samePathsAndByteStrings && fileTypesCorrect && fileSizeCorrect

    return $ and threadResults

-- | Read a fixed sparse file (sparse.tar) using our library and make sure the results are as
-- expected. (The file was created manually on Linux with "cp --sparse=always" to create the sparse
-- files and "tar -Scvf" to create the archive. We were unable to do the equivalent thing on macOS
-- Mojave / APFS.)
testSparse :: TestTree
testSparse = testProperty "sparse" $ monadicIO $ do
  let fileFold =
        F.foldlM'
          ( \(mfp, mbs) e ->
              case e of
                Left h -> do
                  mfp_ <- headerPathName h
                  return (BC.unpack <$> mfp_, mbs)
                Right bs ->
                  return
                    ( mfp,
                      case mbs of
                        Nothing -> Just bs
                        Just bs' -> Just $ bs' `BC.append` bs
                    )
          )
          (return (Nothing, Nothing))

  numThreads <- pick $ chooseInt (1, 4)

  archives <-
    run $
      replicateConcurrently numThreads $
        S.unfold readArchive (id, "test/data/sparse.tar")
          & groupByHeader fileFold
          & fmap (\(mfp, mbs) -> (fromJust mfp, fromJust mbs))
          & S.toList

  threadResults <- forM archives $ \archive -> do
    let validPaths = map fst archive == ["zero", "zeroZero", "zeroAsdf", "asdfZero"]

    let tenMb = 10_000_000
    let zero = B.replicate tenMb 0
    let asdf = "asdf"

    let validByteString1 = snd (head archive) == zero
    let validByteString2 = snd (archive !! 1) == zero `B.append` zero
    let validByteString3 = snd (archive !! 2) == zero `B.append` asdf
    let validByteString4 = snd (archive !! 3) == asdf `B.append` zero

    return $
      and
        [ validPaths,
          validByteString1,
          validByteString2,
          validByteString3,
          validByteString4
        ]

  return $ and threadResults

-- | Writes a given hierarchy of relative paths (created with 'randomHierarchy') to disk in the
-- specified directory and returns the same hierarchy except with actual ByteStrings instead of
-- lengths. Note: The original relative paths are returned back unaltered.
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

-- | Recursively generates a random hierarchy of relative paths to files and directories. (Nothing
-- is written to disk; only the paths are returned.) The initial dirPath should be "". A random
-- bytestring length is provided in case of a file; 'Nothing' in the case of a directory.
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

  return $ zip filePaths bsLengths ++ map (,Nothing) dirPaths ++ recursion

-- | Generates a random path component of length between 1 and 10, e.g., "HO53UVKQ". For
-- compatibility with case-insensitive file systems, uses only one case.
pathComponent :: Gen String
pathComponent = do
  len <- choose (1, 10)
  vectorOf len $
    chr
      <$> frequency
        [ (1, choose (ord 'A', ord 'Z')),
          (1, choose (ord '0', ord '9'))
        ]
