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

import Crypto.Hash
import Data.ByteString (ByteString)
import Data.Function
import Data.Functor
import Data.Maybe
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.Data.Unfold (Unfold)
import Streamly.External.Archive

main :: IO ()
main = do
  -- For each entry in the archive, the readArchive unfold gets us a Header
  -- followed by zero or more ByteStrings containing chunks of file data.
  let readArch :: Unfold IO FilePath (Either Header ByteString) = readArchive

  -- A fold for converting each archive entry (which, as shown above, is a
  -- Header followed by zero or more ByteStrings) into a path and corresponding
  -- SHA-256 hash (Nothing for no data).
  let entryFold :: Fold IO (Either Header ByteString) (String, Maybe String) =
        F.foldlM'
          ( \(mpath, mctx) e ->
              case e of
                Left h -> do
                  mpath' <- headerPathName h
                  return (mpath', mctx)
                Right bs ->
                  return
                    ( mpath,
                      Just . (`hashUpdate` bs) $
                        fromMaybe (hashInit @SHA256) mctx
                    )
          )
          (return (Nothing, Nothing))
          <&> ( \(mpath, mctx) ->
                  ( show $ fromMaybe (error "path expected") mpath,
                    show . hashFinalize <$> mctx
                  )
              )

  -- Execute the stream, grouping at the headers (the Lefts) using the above
  -- fold, and output the paths and SHA-256 hashes along the way.
  S.unfold readArch "/path/to/archive.tar.gz"
    & groupByHeader entryFold
    & S.mapM print
    & S.fold F.drain
```

## Benchmarks

See `./bench/README.md`. Summary (with rough figures from our machine<sup>†</sup>):
 * For 1-byte files, this library has roughly a 70 ns/byte overhead compared to plain Haskell `IO` code, which has roughly a 895 ns/byte overhead compared to plain C.
 * For larger (> 10 KB) files, this library performs just as good as plain Haskell `IO` code, which has roughly a 0.15 ns/byte overhead compared to plain C.

<sup>†</sup> July 2024; NixOS 22.11; Intel i7-12700K (3.6 GHz, 12 cores); Corsair VENGEANCE LPX DDR4 RAM 64GB (2 x 32GB) 3200MHz; Samsung 970 EVO Plus SSD 2TB (M.2 NVMe).
