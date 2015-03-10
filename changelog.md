# Changelog for [`filepath` package](http://hackage.haskell.org/package/filepath)

_Note: below all `FilePath` values are unquoted, so `\\` really means two backslashes._

## 1.4.0.0  *Mar 2015*

  * Bundled with GHC 7.10.1

  * New function: Add `-<.>` as an alias for `replaceExtension`.

  * Semantic change: `joinDrive /foo bar` now returns `/foo/bar`, instead of `/foobar`

  * Semantic change: on Windows, `splitSearchPath File1;\"File 2\"` now returns `[File1,File2]` instead of `[File1,\"File2\"]`

  * Bug fix: on Posix systems, `normalise //home` now returns `/home`, instead of `//home`

  * Bug fix: `normalise /./` now returns `/` on Posix and `\` on Windows, instead of `//` and `\\`

  * Bug fix: `isDrive ""` now returns `False`, instead of `True`

  * Bug fix: on Windows, `dropTrailingPathSeparator /` now returns `/` unchanged, instead of the normalised `\`

  * Bug fix: on Windows, `equalFilePath C:\ C:` now returns `False`, instead of `True`

  * Bug fix: on Windows, `isValid \\\foo` now returns `False`, instead of `True`

  * Bug fix: on Windows, `isValid \\?\D:file` now returns `False`, instead of `True`

  * Bug fix: on Windows, `normalise \` now returns `\` unchanged, instead of `\\`

  * Bug fix: on Windows, `normalise C:.\` now returns `C:`, instead of `C:\\`

  * Bug fix: on Windows, `normalise //server/test` now returns `\\server\test`, instead of `//server/test` unchanged

  * Bug fix: on Windows, `makeRelative / //` now returns `//`, instead of `""`

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

  * Bug fix: `normalise /` now returns `/`, instead of `/.`
