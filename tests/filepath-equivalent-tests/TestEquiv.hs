{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.QuickCheck hiding ((==>))
import TestUtil
import Prelude as P

import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P
import qualified Legacy.System.FilePath.Windows as LW
import qualified Legacy.System.FilePath.Posix as LP


main :: IO ()
main = runTests equivalentTests


equivalentTests :: [(String, Property)]
equivalentTests =
  [
    ( "pathSeparator (windows)"
    , property $ W.pathSeparator == LW.pathSeparator
    )
    ,
    ( "pathSeparators (windows)"
    , property $ W.pathSeparators == LW.pathSeparators
    )
    ,
    ( "isPathSeparator (windows)"
    , property $ \p -> W.isPathSeparator p == LW.isPathSeparator p
    )
    ,
    ( "searchPathSeparator (windows)"
    , property $ W.searchPathSeparator == LW.searchPathSeparator
    )
    ,
    ( "isSearchPathSeparator (windows)"
    , property $ \p -> W.isSearchPathSeparator p == LW.isSearchPathSeparator p
    )
    ,
    ( "extSeparator (windows)"
    , property $ W.extSeparator == LW.extSeparator
    )
    ,
    ( "isExtSeparator (windows)"
    , property $ \p -> W.isExtSeparator p == LW.isExtSeparator p
    )
    ,
    ( "splitSearchPath (windows)"
    , property $ \p -> W.splitSearchPath p == LW.splitSearchPath p
    )
    ,
    ( "splitExtension (windows)"
    , property $ \p -> W.splitExtension p == LW.splitExtension p
    )
    ,
    ( "takeExtension (windows)"
    , property $ \p -> W.takeExtension p == LW.takeExtension p
    )
    ,
    ( "replaceExtension (windows)"
    , property $ \p s -> W.replaceExtension p s == LW.replaceExtension p s
    )
    ,
    ( "dropExtension (windows)"
    , property $ \p -> W.dropExtension p == LW.dropExtension p
    )
    ,
    ( "addExtension (windows)"
    , property $ \p s -> W.addExtension p s == LW.addExtension p s
    )
    ,
    ( "hasExtension (windows)"
    , property $ \p -> W.hasExtension p == LW.hasExtension p
    )
    ,
    ( "splitExtensions (windows)"
    , property $ \p -> W.splitExtensions p == LW.splitExtensions p
    )
    ,
    ( "dropExtensions (windows)"
    , property $ \p -> W.dropExtensions p == LW.dropExtensions p
    )
    ,
    ( "takeExtensions (windows)"
    , property $ \p -> W.takeExtensions p == LW.takeExtensions p
    )
    ,
    ( "replaceExtensions (windows)"
    , property $ \p s -> W.replaceExtensions p s == LW.replaceExtensions p s
    )
    ,
    ( "isExtensionOf (windows)"
    , property $ \p s -> W.isExtensionOf p s == LW.isExtensionOf p s
    )
    ,
    ( "stripExtension (windows)"
    , property $ \p s -> W.stripExtension p s == LW.stripExtension p s
    )
    ,
    ( "splitFileName (windows)"
    , property $ \p -> W.splitFileName p == LW.splitFileName p
    )
    ,
    ( "takeFileName (windows)"
    , property $ \p -> W.takeFileName p == LW.takeFileName p
    )
    ,
    ( "replaceFileName (windows)"
    , property $ \p s -> W.replaceFileName p s == LW.replaceFileName p s
    )
    ,
    ( "dropFileName (windows)"
    , property $ \p -> W.dropFileName p == LW.dropFileName p
    )
    ,
    ( "takeBaseName (windows)"
    , property $ \p -> W.takeBaseName p == LW.takeBaseName p
    )
    ,
    ( "replaceBaseName (windows)"
    , property $ \p s -> W.replaceBaseName p s == LW.replaceBaseName p s
    )
    ,
    ( "takeDirectory (windows)"
    , property $ \p -> W.takeDirectory p == LW.takeDirectory p
    )
    ,
    ( "replaceDirectory (windows)"
    , property $ \p s -> W.replaceDirectory p s == LW.replaceDirectory p s
    )
    ,
    ( "combine (windows)"
    , property $ \p s -> W.combine p s == LW.combine p s
    )
    ,
    ( "splitPath (windows)"
    , property $ \p -> W.splitPath p == LW.splitPath p
    )
    ,
    ( "joinPath (windows)"
    , property $ \p -> W.joinPath p == LW.joinPath p
    )
    ,
    ( "splitDirectories (windows)"
    , property $ \p -> W.splitDirectories p == LW.splitDirectories p
    )
    ,
    ( "splitDirectories (windows)"
    , property $ \p -> W.splitDirectories p == LW.splitDirectories p
    )
    ,
    ( "splitDrive (windows)"
    , property $ \p -> W.splitDrive p == LW.splitDrive p
    )
    ,
    ( "joinDrive (windows)"
    , property $ \p s -> W.joinDrive p s == LW.joinDrive p s
    )
    ,
    ( "takeDrive (windows)"
    , property $ \p -> W.takeDrive p == LW.takeDrive p
    )
    ,
    ( "hasDrive (windows)"
    , property $ \p -> W.hasDrive p == LW.hasDrive p
    )
    ,
    ( "dropDrive (windows)"
    , property $ \p -> W.dropDrive p == LW.dropDrive p
    )
    ,
    ( "isDrive (windows)"
    , property $ \p -> W.isDrive p == LW.isDrive p
    )
    ,
    ( "hasTrailingPathSeparator (windows)"
    , property $ \p -> W.hasTrailingPathSeparator p == LW.hasTrailingPathSeparator p
    )
    ,
    ( "addTrailingPathSeparator (windows)"
    , property $ \p -> W.addTrailingPathSeparator p == LW.addTrailingPathSeparator p
    )
    ,
    ( "dropTrailingPathSeparator (windows)"
    , property $ \p -> W.dropTrailingPathSeparator p == LW.dropTrailingPathSeparator p
    )
    ,
    ( "normalise (windows)"
    , property $ \p -> W.normalise p == LW.normalise p
    )
    ,
    ( "equalFilePath (windows)"
    , property $ \p s -> W.equalFilePath p s == LW.equalFilePath p s
    )
    ,
    ( "makeRelative (windows)"
    , property $ \p s -> W.makeRelative p s == LW.makeRelative p s
    )
    ,
    ( "isRelative (windows)"
    , property $ \p -> W.isRelative p == LW.isRelative p
    )
    ,
    ( "isAbsolute (windows)"
    , property $ \p -> W.isAbsolute p == LW.isAbsolute p
    )
    ,
    ( "isValid (windows)"
    , property $ \p -> W.isValid p == LW.isValid p
    )
    ,
    ( "makeValid (windows)"
    , property $ \p -> W.makeValid p == LW.makeValid p
    )
    ,
    ( "pathSeparator (posix)"
    , property $ P.pathSeparator == LP.pathSeparator
    )
    ,
    ( "pathSeparators (posix)"
    , property $ P.pathSeparators == LP.pathSeparators
    )
    ,
    ( "isPathSeparator (posix)"
    , property $ \p -> P.isPathSeparator p == LP.isPathSeparator p
    )
    ,
    ( "searchPathSeparator (posix)"
    , property $ P.searchPathSeparator == LP.searchPathSeparator
    )
    ,
    ( "isSearchPathSeparator (posix)"
    , property $ \p -> P.isSearchPathSeparator p == LP.isSearchPathSeparator p
    )
    ,
    ( "extSeparator (posix)"
    , property $ P.extSeparator == LP.extSeparator
    )
    ,
    ( "isExtSeparator (posix)"
    , property $ \p -> P.isExtSeparator p == LP.isExtSeparator p
    )
    ,
    ( "splitSearchPath (posix)"
    , property $ \p -> P.splitSearchPath p == LP.splitSearchPath p
    )
    ,
    ( "splitExtension (posix)"
    , property $ \p -> P.splitExtension p == LP.splitExtension p
    )
    ,
    ( "takeExtension (posix)"
    , property $ \p -> P.takeExtension p == LP.takeExtension p
    )
    ,
    ( "replaceExtension (posix)"
    , property $ \p s -> P.replaceExtension p s == LP.replaceExtension p s
    )
    ,
    ( "dropExtension (posix)"
    , property $ \p -> P.dropExtension p == LP.dropExtension p
    )
    ,
    ( "addExtension (posix)"
    , property $ \p s -> P.addExtension p s == LP.addExtension p s
    )
    ,
    ( "hasExtension (posix)"
    , property $ \p -> P.hasExtension p == LP.hasExtension p
    )
    ,
    ( "splitExtensions (posix)"
    , property $ \p -> P.splitExtensions p == LP.splitExtensions p
    )
    ,
    ( "dropExtensions (posix)"
    , property $ \p -> P.dropExtensions p == LP.dropExtensions p
    )
    ,
    ( "takeExtensions (posix)"
    , property $ \p -> P.takeExtensions p == LP.takeExtensions p
    )
    ,
    ( "replaceExtensions (posix)"
    , property $ \p s -> P.replaceExtensions p s == LP.replaceExtensions p s
    )
    ,
    ( "isExtensionOf (posix)"
    , property $ \p s -> P.isExtensionOf p s == LP.isExtensionOf p s
    )
    ,
    ( "stripExtension (posix)"
    , property $ \p s -> P.stripExtension p s == LP.stripExtension p s
    )
    ,
    ( "splitFileName (posix)"
    , property $ \p -> P.splitFileName p == LP.splitFileName p
    )
    ,
    ( "takeFileName (posix)"
    , property $ \p -> P.takeFileName p == LP.takeFileName p
    )
    ,
    ( "replaceFileName (posix)"
    , property $ \p s -> P.replaceFileName p s == LP.replaceFileName p s
    )
    ,
    ( "dropFileName (posix)"
    , property $ \p -> P.dropFileName p == LP.dropFileName p
    )
    ,
    ( "takeBaseName (posix)"
    , property $ \p -> P.takeBaseName p == LP.takeBaseName p
    )
    ,
    ( "replaceBaseName (posix)"
    , property $ \p s -> P.replaceBaseName p s == LP.replaceBaseName p s
    )
    ,
    ( "takeDirectory (posix)"
    , property $ \p -> P.takeDirectory p == LP.takeDirectory p
    )
    ,
    ( "replaceDirectory (posix)"
    , property $ \p s -> P.replaceDirectory p s == LP.replaceDirectory p s
    )
    ,
    ( "combine (posix)"
    , property $ \p s -> P.combine p s == LP.combine p s
    )
    ,
    ( "splitPath (posix)"
    , property $ \p -> P.splitPath p == LP.splitPath p
    )
    ,
    ( "joinPath (posix)"
    , property $ \p -> P.joinPath p == LP.joinPath p
    )
    ,
    ( "splitDirectories (posix)"
    , property $ \p -> P.splitDirectories p == LP.splitDirectories p
    )
    ,
    ( "splitDirectories (posix)"
    , property $ \p -> P.splitDirectories p == LP.splitDirectories p
    )
    ,
    ( "splitDrive (posix)"
    , property $ \p -> P.splitDrive p == LP.splitDrive p
    )
    ,
    ( "joinDrive (posix)"
    , property $ \p s -> P.joinDrive p s == LP.joinDrive p s
    )
    ,
    ( "takeDrive (posix)"
    , property $ \p -> P.takeDrive p == LP.takeDrive p
    )
    ,
    ( "hasDrive (posix)"
    , property $ \p -> P.hasDrive p == LP.hasDrive p
    )
    ,
    ( "dropDrive (posix)"
    , property $ \p -> P.dropDrive p == LP.dropDrive p
    )
    ,
    ( "isDrive (posix)"
    , property $ \p -> P.isDrive p == LP.isDrive p
    )
    ,
    ( "hasTrailingPathSeparator (posix)"
    , property $ \p -> P.hasTrailingPathSeparator p == LP.hasTrailingPathSeparator p
    )
    ,
    ( "addTrailingPathSeparator (posix)"
    , property $ \p -> P.addTrailingPathSeparator p == LP.addTrailingPathSeparator p
    )
    ,
    ( "dropTrailingPathSeparator (posix)"
    , property $ \p -> P.dropTrailingPathSeparator p == LP.dropTrailingPathSeparator p
    )
    ,
    ( "normalise (posix)"
    , property $ \p -> P.normalise p == LP.normalise p
    )
    ,
    ( "equalFilePath (posix)"
    , property $ \p s -> P.equalFilePath p s == LP.equalFilePath p s
    )
    ,
    ( "makeRelative (posix)"
    , property $ \p s -> P.makeRelative p s == LP.makeRelative p s
    )
    ,
    ( "isRelative (posix)"
    , property $ \p -> P.isRelative p == LP.isRelative p
    )
    ,
    ( "isAbsolute (posix)"
    , property $ \p -> P.isAbsolute p == LP.isAbsolute p
    )
    ,
    ( "isValid (posix)"
    , property $ \p -> P.isValid p == LP.isValid p
    )
    ,
    ( "makeValid (posix)"
    , property $ \p -> P.makeValid p == LP.makeValid p
    )
  ]













