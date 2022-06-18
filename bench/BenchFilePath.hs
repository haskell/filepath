{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import System.AbstractFilePath.Types
import System.AbstractFilePath.Encoding ( ucs2le )
import qualified System.OsString.Internal.Types as OST
import qualified Data.ByteString.Short as SBS

import Test.Tasty.Bench

import qualified System.FilePath.Posix as PF
import qualified System.FilePath.Posix as WF
import qualified System.OsString.Posix as OSP
import qualified System.OsString.Windows as WSP
import qualified System.AbstractFilePath.Posix as APF
import qualified System.AbstractFilePath.Windows as AWF


main :: IO ()
main = do
  defaultMain
    [ bgroup "filepath (string)"
      [ bench "splitExtension (posix)"      $ nf PF.splitExtension posixPath
      , bench "splitExtension (windows)"    $ nf WF.splitExtension windowsPath
      , bench "takeExtension (posix)"       $ nf PF.takeExtension posixPath
      , bench "takeExtension (windows)"     $ nf WF.takeExtension windowsPath
      , bench "replaceExtension (posix)"    $ nf (PF.replaceExtension ".lol") posixPath
      , bench "replaceExtension (windows)"  $ nf (WF.replaceExtension ".lol") windowsPath
      , bench "dropExtension (posix)"       $ nf PF.dropExtension posixPath
      , bench "dropExtension (windows)"     $ nf WF.dropExtension windowsPath
      , bench "addExtension (posix)"        $ nf (PF.addExtension ".lol") posixPath
      , bench "addExtension (windows)"      $ nf (WF.addExtension ".lol") windowsPath
      , bench "hasExtension (posix)"        $ nf PF.hasExtension posixPath
      , bench "hasExtension (windows)"      $ nf WF.hasExtension windowsPath
      , bench "splitExtensions (posix)"     $ nf PF.splitExtensions posixPath
      , bench "splitExtensions (windows)"   $ nf WF.splitExtensions windowsPath
      , bench "dropExtensions (posix)"      $ nf PF.dropExtensions posixPath
      , bench "dropExtensions (windows)"    $ nf WF.dropExtensions windowsPath
      , bench "takeExtensions (posix)"      $ nf PF.takeExtensions posixPath
      , bench "takeExtensions (windows)"    $ nf WF.takeExtensions windowsPath
      , bench "replaceExtensions (posix)"   $ nf (PF.replaceExtensions ".lol") posixPath
      , bench "replaceExtensions (windows)" $ nf (WF.replaceExtensions ".lol") windowsPath
      , bench "isExtensionOf (posix)"       $ nf (PF.isExtensionOf ".lol") posixPath
      , bench "isExtensionOf (windows)"     $ nf (WF.isExtensionOf ".lol") windowsPath
      , bench "stripExtension (posix)"      $ nf (PF.stripExtension ".lol") posixPath
      , bench "stripExtension (windows)"    $ nf (WF.stripExtension ".lol") windowsPath

      , bench "splitFileName (posix)"       $ nf PF.splitFileName posixPath
      , bench "splitFileName (windows)"     $ nf WF.splitFileName windowsPath
      , bench "takeFileName (posix)"        $ nf PF.takeFileName posixPath
      , bench "takeFileName (windows)"      $ nf WF.takeFileName windowsPath
      , bench "replaceFileName (posix)"     $ nf (PF.replaceFileName "lol") posixPath
      , bench "replaceFileName (windows)"   $ nf (WF.replaceFileName "lol") windowsPath
      , bench "dropFileName (posix)"        $ nf PF.dropFileName posixPath
      , bench "dropFileName (windows)"      $ nf WF.dropFileName windowsPath
      , bench "takeBaseName (posix)"        $ nf PF.takeBaseName posixPath
      , bench "takeBaseName (windows)"      $ nf WF.takeBaseName windowsPath
      , bench "replaceBaseName (posix)"     $ nf (PF.replaceBaseName "lol") posixPath
      , bench "replaceBaseName (windows)"   $ nf (WF.replaceBaseName "lol") windowsPath
      , bench "takeDirectory (posix)"       $ nf PF.takeDirectory posixPath
      , bench "takeDirectory (windows)"     $ nf WF.takeDirectory windowsPath
      , bench "replaceDirectory (posix)"    $ nf (PF.replaceDirectory "lol") posixPath
      , bench "replaceDirectory (windows)"  $ nf (WF.replaceDirectory "lol") windowsPath
      , bench "combine (posix)"             $ nf (PF.combine "lol") posixPath
      , bench "combine (windows)"           $ nf (WF.combine "lol") windowsPath
      , bench "splitPath (posix)"           $ nf PF.splitPath    posixPath
      , bench "splitPath (windows)"         $ nf WF.splitPath    windowsPath
      , bench "joinPath (posix)"            $ nf PF.joinPath     (PF.splitPath posixPath)
      , bench "joinPath (windows)"          $ nf WF.joinPath     (WF.splitPath windowsPath)
      , bench "splitDirectories (posix)"    $ nf PF.splitDirectories    posixPath
      , bench "splitDirectories (windows)"  $ nf WF.splitDirectories    windowsPath

      , bench "splitDrive (posix)"          $ nf PF.splitDrive    posixPath
      , bench "splitDrive (windows)"        $ nf WF.splitDrive    windowsPath
      , bench "joinDrive (posix)"           $ nf (PF.joinDrive "/")    posixPath
      , bench "joinDrive (windows)"         $ nf (WF.joinDrive "C:\\")  windowsPath
      , bench "takeDrive (posix)"           $ nf PF.takeDrive    posixPath
      , bench "takeDrive (windows)"         $ nf WF.takeDrive    windowsPath
      , bench "hasDrive (posix)"            $ nf PF.hasDrive    posixPath
      , bench "hasDrive (windows)"          $ nf WF.hasDrive    windowsPath
      , bench "dropDrive (posix)"           $ nf PF.dropDrive    posixPath
      , bench "dropDrive (windows)"         $ nf WF.dropDrive    windowsPath
      , bench "isDrive (posix)"             $ nf PF.isDrive    posixPath
      , bench "isDrive (windows)"           $ nf WF.isDrive    windowsPath

      , bench "hasTrailingPathSeparator (posix)"    $ nf PF.hasTrailingPathSeparator    posixPath
      , bench "hasTrailingPathSeparator (windows)"  $ nf WF.hasTrailingPathSeparator    windowsPath
      , bench "addTrailingPathSeparator (posix)"    $ nf PF.addTrailingPathSeparator    posixPath
      , bench "addTrailingPathSeparator (windows)"  $ nf WF.addTrailingPathSeparator    windowsPath
      , bench "dropTrailingPathSeparator (posix)"   $ nf PF.addTrailingPathSeparator    posixPath
      , bench "dropTrailingPathSeparator (windows)" $ nf WF.addTrailingPathSeparator    windowsPath

      , bench "normalise (posix)"           $ nf PF.normalise    posixPath
      , bench "normalise (windows)"         $ nf WF.normalise    windowsPath
      , bench "equalFilePath (posix)"       $ nf (PF.equalFilePath "abc/def/zs")   posixPath
      , bench "equalFilePath (windows)"     $ nf (WF.equalFilePath "abc/def/zs")   windowsPath
      , bench "makeRelative (posix)"        $ nf (PF.makeRelative "abc/def/zs")   posixPath
      , bench "makeRelative (windows)"      $ nf (WF.makeRelative "abc/def/zs")   windowsPath
      , bench "isRelative (posix)"          $ nf PF.isRelative    posixPath
      , bench "isRelative (windows)"        $ nf WF.isRelative    windowsPath
      , bench "isAbsolute (posix)"          $ nf PF.isAbsolute    posixPath
      , bench "isAbsolute (windows)"        $ nf WF.isAbsolute    windowsPath
      , bench "isValid (posix)"             $ nf PF.isValid    posixPath
      , bench "isValid (windows)"           $ nf WF.isValid    windowsPath
      , bench "makeValid (posix)"           $ nf PF.makeValid    posixPath
      , bench "makeValid (windows)"         $ nf WF.makeValid    windowsPath

      , bench "splitSearchPath (posix)"    $ nf PF.splitSearchPath posixSearchPath
      , bench "splitSearchPath (windows)"  $ nf WF.splitSearchPath windowsSearchPath
      ]

    , bgroup "filepath (AFPP)"
      [ bench "splitExtension (posix)"      $ nf APF.splitExtension posixPathAFPP
      , bench "splitExtension (windows)"    $ nf AWF.splitExtension windowsPathAFPP
      , bench "takeExtension (posix)"       $ nf APF.takeExtension posixPathAFPP
      , bench "takeExtension (windows)"     $ nf AWF.takeExtension windowsPathAFPP
      , bench "replaceExtension (posix)"    $ nf (APF.replaceExtension [OSP.pstr|.lol|]) posixPathAFPP
      , bench "replaceExtension (windows)"  $ nf (AWF.replaceExtension [WSP.pstr|.lol|]) windowsPathAFPP
      , bench "dropExtension (posix)"       $ nf APF.dropExtension posixPathAFPP
      , bench "dropExtension (windows)"     $ nf AWF.dropExtension windowsPathAFPP
      , bench "addExtension (posix)"        $ nf (APF.addExtension [OSP.pstr|.lol|]) posixPathAFPP
      , bench "addExtension (windows)"      $ nf (AWF.addExtension [WSP.pstr|.lol|]) windowsPathAFPP
      , bench "hasExtension (posix)"        $ nf APF.hasExtension posixPathAFPP
      , bench "hasExtension (windows)"      $ nf AWF.hasExtension windowsPathAFPP
      , bench "splitExtensions (posix)"     $ nf APF.splitExtensions posixPathAFPP
      , bench "splitExtensions (windows)"   $ nf AWF.splitExtensions windowsPathAFPP
      , bench "dropExtensions (posix)"      $ nf APF.dropExtensions posixPathAFPP
      , bench "dropExtensions (windows)"    $ nf AWF.dropExtensions windowsPathAFPP
      , bench "takeExtensions (posix)"      $ nf APF.takeExtensions posixPathAFPP
      , bench "takeExtensions (windows)"    $ nf AWF.takeExtensions windowsPathAFPP
      , bench "replaceExtensions (posix)"   $ nf (APF.replaceExtensions [OSP.pstr|.lol|]) posixPathAFPP
      , bench "replaceExtensions (windows)" $ nf (AWF.replaceExtensions [WSP.pstr|.lol|]) windowsPathAFPP
      , bench "isExtensionOf (posix)"       $ nf (APF.isExtensionOf [OSP.pstr|.lol|]) posixPathAFPP
      , bench "isExtensionOf (windows)"     $ nf (AWF.isExtensionOf [WSP.pstr|.lol|]) windowsPathAFPP
      , bench "stripExtension (posix)"      $ nf (APF.stripExtension [OSP.pstr|.lol|]) posixPathAFPP
      , bench "stripExtension (windows)"    $ nf (AWF.stripExtension [WSP.pstr|.lol|]) windowsPathAFPP

      , bench "splitFileName (posix)"       $ nf APF.splitFileName posixPathAFPP
      , bench "splitFileName (windows)"     $ nf AWF.splitFileName windowsPathAFPP
      , bench "takeFileName (posix)"        $ nf APF.takeFileName posixPathAFPP
      , bench "takeFileName (windows)"      $ nf AWF.takeFileName windowsPathAFPP
      , bench "replaceFileName (posix)"     $ nf (APF.replaceFileName [OSP.pstr|lol|]) posixPathAFPP
      , bench "replaceFileName (windows)"   $ nf (AWF.replaceFileName [WSP.pstr|lol|]) windowsPathAFPP
      , bench "dropFileName (posix)"        $ nf APF.dropFileName posixPathAFPP
      , bench "dropFileName (windows)"      $ nf AWF.dropFileName windowsPathAFPP
      , bench "takeBaseName (posix)"        $ nf APF.takeBaseName posixPathAFPP
      , bench "takeBaseName (windows)"      $ nf AWF.takeBaseName windowsPathAFPP
      , bench "replaceBaseName (posix)"     $ nf (APF.replaceBaseName [OSP.pstr|lol|]) posixPathAFPP
      , bench "replaceBaseName (windows)"   $ nf (AWF.replaceBaseName [WSP.pstr|lol|]) windowsPathAFPP
      , bench "takeDirectory (posix)"       $ nf APF.takeDirectory posixPathAFPP
      , bench "takeDirectory (windows)"     $ nf AWF.takeDirectory windowsPathAFPP
      , bench "replaceDirectory (posix)"    $ nf (APF.replaceDirectory [OSP.pstr|lol|]) posixPathAFPP
      , bench "replaceDirectory (windows)"  $ nf (AWF.replaceDirectory [WSP.pstr|lol|]) windowsPathAFPP
      , bench "combine (posix)"             $ nf (APF.combine [OSP.pstr|lol|]) posixPathAFPP
      , bench "combine (windows)"           $ nf (AWF.combine [WSP.pstr|lol|]) windowsPathAFPP
      , bench "splitPath (posix)"           $ nf APF.splitPath    posixPathAFPP
      , bench "splitPath (windows)"         $ nf AWF.splitPath    windowsPathAFPP
      , bench "joinPath (posix)"            $ nf APF.joinPath     (APF.splitPath posixPathAFPP)
      , bench "joinPath (windows)"          $ nf AWF.joinPath     (AWF.splitPath windowsPathAFPP)
      , bench "splitDirectories (posix)"    $ nf APF.splitDirectories    posixPathAFPP
      , bench "splitDirectories (windows)"  $ nf AWF.splitDirectories    windowsPathAFPP

      , bench "splitDrive (posix)"          $ nf APF.splitDrive    posixPathAFPP
      , bench "splitDrive (windows)"        $ nf AWF.splitDrive    windowsPathAFPP
      , bench "joinDrive (posix)"           $ nf (APF.joinDrive [OSP.pstr|/|])    posixPathAFPP
      , bench "joinDrive (windows)"         $ nf (AWF.joinDrive [WSP.pstr|C:\|])    windowsPathAFPP
      , bench "takeDrive (posix)"           $ nf APF.takeDrive    posixPathAFPP
      , bench "takeDrive (windows)"         $ nf AWF.takeDrive    windowsPathAFPP
      , bench "hasDrive (posix)"            $ nf APF.hasDrive    posixPathAFPP
      , bench "hasDrive (windows)"          $ nf AWF.hasDrive    windowsPathAFPP
      , bench "dropDrive (posix)"           $ nf APF.dropDrive    posixPathAFPP
      , bench "dropDrive (windows)"         $ nf AWF.dropDrive    windowsPathAFPP
      , bench "isDrive (posix)"             $ nf APF.isDrive    posixPathAFPP
      , bench "isDrive (windows)"           $ nf AWF.isDrive    windowsPathAFPP

      , bench "hasTrailingPathSeparator (posix)"    $ nf APF.hasTrailingPathSeparator    posixPathAFPP
      , bench "hasTrailingPathSeparator (windows)"  $ nf AWF.hasTrailingPathSeparator    windowsPathAFPP
      , bench "addTrailingPathSeparator (posix)"    $ nf APF.addTrailingPathSeparator    posixPathAFPP
      , bench "addTrailingPathSeparator (windows)"  $ nf AWF.addTrailingPathSeparator    windowsPathAFPP
      , bench "dropTrailingPathSeparator (posix)"   $ nf APF.addTrailingPathSeparator    posixPathAFPP
      , bench "dropTrailingPathSeparator (windows)" $ nf AWF.addTrailingPathSeparator    windowsPathAFPP

      , bench "normalise (posix)"           $ nf APF.normalise    posixPathAFPP
      , bench "normalise (windows)"         $ nf AWF.normalise    windowsPathAFPP
      , bench "equalFilePath (posix)"       $ nf (APF.equalFilePath [OSP.pstr|abc/def/zs|])   posixPathAFPP
      , bench "equalFilePath (windows)"     $ nf (AWF.equalFilePath [WSP.pstr|abc/def/zs|])   windowsPathAFPP
      , bench "makeRelative (posix)"        $ nf (APF.makeRelative [OSP.pstr|abc/def/zs|])   posixPathAFPP
      , bench "makeRelative (windows)"      $ nf (AWF.makeRelative [WSP.pstr|abc/def/zs|])   windowsPathAFPP
      , bench "isRelative (posix)"          $ nf APF.isRelative    posixPathAFPP
      , bench "isRelative (windows)"        $ nf AWF.isRelative    windowsPathAFPP
      , bench "isAbsolute (posix)"          $ nf APF.isAbsolute    posixPathAFPP
      , bench "isAbsolute (windows)"        $ nf AWF.isAbsolute    windowsPathAFPP
      , bench "isValid (posix)"             $ nf APF.isValid    posixPathAFPP
      , bench "isValid (windows)"           $ nf AWF.isValid    windowsPathAFPP
      , bench "makeValid (posix)"           $ nf APF.makeValid    posixPathAFPP
      , bench "makeValid (windows)"         $ nf AWF.makeValid    windowsPathAFPP

      , bench "splitSearchPath (posix)"     $ nf APF.splitSearchPath posixSearchPathAFPP
      , bench "splitSearchPath (windows)"   $ nf AWF.splitSearchPath windowsSearchPathAFPP
      ]

    , bgroup "encoding/decoding"
      [ bench "fromPlatformStringUtf (posix)"      $ nf (APF.fromPlatformStringUtf @Maybe) posixPathAFPP
      , bench "fromPlatformStringUtf (windows)"    $ nf (AWF.fromPlatformStringUtf @Maybe) windowsPathAFPP
      , bench "fromPlatformStringEnc (windows)"    $ nf (AWF.fromPlatformStringEnc ucs2le) windowsPathAFPP

      , bench "toPlatformStringUtf (posix)"        $ nf (APF.toPlatformStringUtf @Maybe) posixPath
      , bench "toPlatformStringUtf (windows)"      $ nf (AWF.toPlatformStringUtf @Maybe) windowsPath
      , bench "toPlatformStringEnc (windows)"      $ nf (AWF.toPlatformStringEnc ucs2le) windowsPath

      , bench "unpackPlatformString (posix)"       $ nf APF.unpackPlatformString posixPathAFPP
      , bench "unpackPlatformString (windows)"     $ nf AWF.unpackPlatformString windowsPathAFPP
      , bench "packPlatformString (posix)"         $ nf APF.packPlatformString (APF.unpackPlatformString posixPathAFPP)
      , bench "packPlatformString (windows)"       $ nf AWF.packPlatformString (AWF.unpackPlatformString windowsPathAFPP)

      , bench "bytesToPlatformString (posix)"      $ nf (OSP.bytesToPlatformString @Maybe) (SBS.fromShort . OST.unPFP $ posixPathAFPP)
      , bench "bytesToPlatformString (windows)"    $ nf (WSP.bytesToPlatformString @Maybe) (SBS.fromShort . OST.unWFP $ windowsPathAFPP)
      ]
    ]


posixPath :: FilePath
posixPath = "/foo/bar/bath/baz/baz/tz/fooooooooooooooo/laaaaaaaaaaaaaaa/baaaaaaaaaaaaar/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk/kkkkkkkkkkkkkkkkkk/h/h/h/a/s/r/a/h/gt/r/r/r/s/s.txt"

windowsPath :: FilePath
windowsPath = "C:\\foo\\bar\\bath\\baz\\baz\\tz\\fooooooooooooooo\\laaaaaaaaaaaaaaa\\baaaaaaaaaaaaar\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk\\kkkkkkkkkkkkkkkkkk\\h\\h\\h\\a\\s\\r\\a\\h\\gt\\r\\r\\r\\s\\s.txt"

posixPathAFPP :: PosixFilePath
posixPathAFPP = [OSP.pstr|/foo/bar/bath/baz/baz/tz/fooooooooooooooo/laaaaaaaaaaaaaaa/baaaaaaaaaaaaar/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz/kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk/kkkkkkkkkkkkkkkkkk/h/h/h/a/s/r/a/h/gt/r/r/r/s/s.txt|]

windowsPathAFPP :: WindowsFilePath
windowsPathAFPP = [WSP.pstr|C:\\foo\\bar\\bath\\baz\\baz\\tz\\fooooooooooooooo\\laaaaaaaaaaaaaaa\\baaaaaaaaaaaaar\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz\\kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk\\kkkkkkkkkkkkkkkkkk\\h\\h\\h\\a\\s\\r\\a\\h\\gt\\r\\r\\r\\s\\s.txt|]

posixSearchPath :: FilePath
posixSearchPath = ":foo:bar:bath:baz:baz:tz:fooooooooooooooo:laaaaaaaaaaaaaaa:baaaaaaaaaaaaar:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk:kkkkkkkkkkkkkkkkkk:h:h:h:a:s:r:a:h:gt:r:r:r:s:s.txt"

windowsSearchPath :: FilePath
windowsSearchPath = "foo;bar;bath;baz;baz;tz;fooooooooooooooo;laaaaaaaaaaaaaaa;baaaaaaaaaaaaar;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk;kkkkkkkkkkkkkkkkkk;h;h;h;a;s;r;a;h;gt;r;r;r;s;s.txt"

posixSearchPathAFPP :: PosixString
posixSearchPathAFPP = [OSP.pstr|:foo:bar:bath:baz:baz:tz:fooooooooooooooo:laaaaaaaaaaaaaaa:baaaaaaaaaaaaar:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz:kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk:kkkkkkkkkkkkkkkkkk:h:h:h:a:s:r:a:h:gt:r:r:r:s:s.txt|]

windowsSearchPathAFPP :: WindowsString
windowsSearchPathAFPP = [WSP.pstr|foo;bar;bath;baz;baz;tz;fooooooooooooooo;laaaaaaaaaaaaaaa;baaaaaaaaaaaaar;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz;kkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkkk;kkkkkkkkkkkkkkkkkk;h;h;h;a;s;r;a;h;gt;r;r;r;s;s.txt|]
