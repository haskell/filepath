# Changelog for [`filepath` package](http://hackage.haskell.org/package/filepath)

## 1.3.0.3  *TBA*

  * Bundled with GHC 7.10.1

  * Semantic change: `joinDrive "/foo" "bar"` now returns `"/foo/bar"`,
    instead of `"/foobar"`.

  * Bug fix: `isDrive ""` now retuns `False`, instead of `True`.

  * Bug fix: on Windows, `dropTrailingPathSeparator "/"` now returns `"/"`
    unchanged, instead of the normalised `"\\"`.

  * Bug fix: on Windows, `equalFilePath "C:\\" "C:"` now retuns `False`,
    instead of `True`.

  * Bug fix: on Windows, `isValid "\\\\\\foo"` now returns `False`, instead
    of `True`.

  * Bug fix: on Windows, `normalise "\\"` now retuns `"\\"` unchanged,
    instead of `"\\\\"`.

  * Bug fix: on Windows, `normalise "//server/test"` now retuns
    `"\\\\server\\test"`, instead of `"//server/test"` unchanged.

  * Bug fix: on Windows, `makeRelative "/" "//"` now returns `"//"`, instead
    of `""`.

## 1.3.0.2  *Mar 2014*

  * Bundled with GHC 7.8.1

  * Update to Cabal 1.10 format

  * Minor Haddock cleanups

## 1.3.0.1  *Sep 2012*

  * Bundled with GHC 7.6.1

  * No changes

## 1.3.0.0  *Feb 2012*

  * Bundled with GHC 7.4.1

  * Add support for SafeHaskell

  * Fix `normalise "/"` to result in `"/"` rather than `"/."`
