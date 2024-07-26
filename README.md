Haskell MPFR binding

### System
Sonoma 14.5 Apple M1 (Darwin)

### Notes - what is needed to work 
**GHC 9.6.6 plus Cabal 3.10.3.0 plus Stack 2.15.7 plus CLANG 16 ** ; 

Homebrew clang version 16.0.6
Target: arm64-apple-darwin23.5.0
Thread model: posix
InstalledDir: /usr/local/bin

### Libraries 
mpfr 4.2.1, icu4c 74.2, gmp 6.3.0 (use brew to install in each case)

## Specific
-- Note the changes in cabal for demo; have gcc point to clang16 (use sudo ln -sf /opt/homebrew/opt/llvm@16/bin/clang-16 /usr/     local/bin/gcc) ; ensure mpfr's path (use brew list mpfr) is on the path. Ditto with gmp.
-- GHC version greater than what's noted above do not work

