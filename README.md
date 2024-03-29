# streamly-archive

[![Hackage](https://img.shields.io/hackage/v/streamly-archive.svg?style=flat)](https://hackage.haskell.org/package/streamly-archive)
![CI](https://github.com/shlok/streamly-archive/workflows/CI/badge.svg?branch=master)

Stream data from archives (tar, tar.gz, zip, or any other format [supported by libarchive](https://github.com/libarchive/libarchive/wiki/LibarchiveFormats)) using the Haskell [streamly](https://hackage.haskell.org/package/streamly) library.

## Requirements

Install libarchive on your system.

* Debian Linux: `sudo apt-get install libarchive-dev`.
* macOS: `brew install libarchive`.

## Quick start

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Crypto.Hash (hashFinalize, hashInit, hashUpdate)
import Crypto.Hash.Algorithms (SHA256)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe (fromJust, fromMaybe)
import Data.Void (Void)
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Archive
  ( Header,
    groupByHeader,
    headerPathName,
    readArchive,
  )
import Streamly.Internal.Data.Fold.Type (Fold (Fold), Step (Partial))
import Streamly.Internal.Data.Unfold.Type (Unfold)

main :: IO ()
main = do
  -- Obtain an unfold for the archive.
  -- For each entry in the archive, we will get a Header followed
  -- by zero or more ByteStrings containing chunks of file data.
  let unf :: Unfold IO Void (Either Header ByteString) =
        readArchive "/path/to/archive.tar.gz"

  -- Create a fold for converting each entry (which, as we saw
  -- above, is a Left followed by zero or more Rights) into a
  -- path and corresponding SHA-256 hash (Nothing for no data).
  let entryFold :: Fold IO (Either Header ByteString) (String, Maybe String) =
        Fold
          ( \(mpath, mctx) e ->
              case e of
                Left h -> do
                  mpath' <- headerPathName h
                  return $ Partial (mpath', mctx)
                Right bs ->
                  return $
                    Partial
                      ( mpath,
                        Just . (`hashUpdate` bs) $
                          fromMaybe (hashInit @SHA256) mctx
                      )
          )
          (return $ Partial (Nothing, Nothing))
          ( \(mpath, mctx) ->
              return
                ( show $ fromJust mpath,
                  show . hashFinalize <$> mctx
                )
          )

  -- Execute the stream, grouping at the headers (the Lefts) using the
  -- above fold, and output the paths and SHA-256 hashes along the way.
  S.unfold unf undefined
    & groupByHeader entryFold
    & S.mapM print
    & S.fold F.drain
```

## Benchmarks

See `./bench/README.md`. We find on our machine<sup>†</sup> that (1) reading an archive using this library is just as fast as using plain Haskell `IO` code; and that (2) both are somewhere between 1.7x (large files) and 2.5x (many 1-byte files) slower than C.

The former fulfills the promise of [streamly](https://hackage.haskell.org/package/streamly) and stream fusion. The differences to C are presumably explained by the marshalling of data into the Haskell world and are currently small enough for our purposes.

<sup>†</sup> April 2023; [Linode](https://linode.com); Debian 11, Dedicated 32GB: 16 CPU, 640GB Storage, 32GB RAM.
