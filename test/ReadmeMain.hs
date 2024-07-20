{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module ReadmeMain where

import Crypto.Hash
import Data.ByteString (ByteString)
import Data.Function
import Data.Functor
import Data.Maybe
import Streamly.Data.Fold (Fold)
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Stream.Prelude as S
import Streamly.External.Archive

main :: IO ()
main = do
  -- A fold for converting each archive entry (which is a Header followed by
  -- zero or more ByteStrings) into a path and corresponding SHA-256 hash
  -- (Nothing for no data).
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
  S.unfold readArchive (id, "/path/to/archive.tar.gz")
    & groupByLefts entryFold
    & S.mapM print
    & S.fold F.drain
