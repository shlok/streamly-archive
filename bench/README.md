# streamly-archive benchmarks

This directory contains three command-line programs for accessing an archive:

1. `bench-archive.c`: A plain C program.
2. `bench-archive-plain.hs`: A plain Haskell program (plain `IO`-monadic code) that has the same functionality as the C
   program.
3. `bench-archive-streamly.hs`: A Haskell program that has the same functionality but uses our library (streamly-archive).

## Running benchmarks

* Install [Stack](https://docs.haskellstack.org/en/stable/README/).
* Install libarchive on your system. Debian Linux: `sudo apt-get install libarchive-dev`. macOS: `brew install
  libarchive`.
* Install other necessary programs. Debian Linux: `sudo apt-get install bc pcregrep`. macOS: `brew install
  gnu-time TODO`.
* Run `stack build` and `stack exec -- bench`.
* View the `csv` file. We are currently not explaining the columns in detail here, but our main conclusions are outlined
  in `../README.md`.
