# FilePath [![Hackage version](https://img.shields.io/hackage/v/filepath.svg?style=flat)](http://hackage.haskell.org/package/filepath) [![Build Status](http://img.shields.io/travis/haskell/filepath.svg?style=flat)](https://travis-ci.org/haskell/filepath)

The `filepath` package provides functionality for manipulating `FilePath` values, and is shipped with both [GHC](https://www.haskell.org/ghc/) and the [Haskell Platform](https://www.haskell.org/platform/). It provides three modules:

* [`System.FilePath.Posix`](http://hackage.haskell.org/package/filepath/docs/System-FilePath-Posix.html) manipulates POSIX/Linux style `FilePath` values (with `/` as the path separator).
* [`System.FilePath.Windows`](http://hackage.haskell.org/package/filepath/docs/System-FilePath-Windows.html) manipulates Windows style `FilePath` values (with either `/` or `\` as the path separator, and dealing with drives).
* [`System.FilePath`](http://hackage.haskell.org/package/filepath/docs/System-FilePath.html) which is an alias for the module appropriate to your platform.

All three modules provide the same API, and the same documentation (calling out differences in the different variants).
