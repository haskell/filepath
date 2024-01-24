{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Test.Tasty
import Test.Tasty.QuickCheck hiding ((==>))
import TestUtil
import Prelude as P
import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List.NonEmpty (NonEmpty(..))
import Generic.Random
import Generics.Deriving.Show
import GHC.Generics

import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P
import qualified Legacy.System.FilePath.Windows as LW
import qualified Legacy.System.FilePath.Posix as LP
import qualified Data.List.NonEmpty as NE


class AltShow a where
  altShow :: a -> String

instance {-# OVERLAPPABLE #-} Show a => AltShow a where
  altShow = show

instance {-# OVERLAPS #-} AltShow String where
  altShow = id

instance {-# OVERLAPPABLE #-} AltShow a => AltShow (Maybe a) where
  altShow Nothing = ""
  altShow (Just a) = altShow a


newtype WindowsFilePaths = WindowsFilePaths { unWindowsFilePaths :: [WindowsFilePath] }
  deriving (Show, Eq, Ord, Generic)

-- filepath = namespace *"\" namespace-tail
--          / UNC
--          / [ disk ] *"\" relative-path
--          / disk *"\"
data WindowsFilePath = NS NameSpace [Separator] NSTail
                     | UNC UNCShare
                     | N (Maybe Char) [Separator] (Maybe RelFilePath)
                     -- ^ This differs from the grammar, because we allow
                     -- empty paths
                     | PotentiallyInvalid FilePath
                     -- ^ this branch is added purely for the tests
  deriving (GShow, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` WindowsFilePath)

instance Show WindowsFilePath where
  show wf = gshow wf ++ " (" ++ altShow wf ++ ")"

instance AltShow WindowsFilePath where
  altShow (NS ns seps nstail) = altShow ns ++ altShow seps ++ altShow nstail
  altShow (UNC unc) = altShow unc
  altShow (N mdisk seps mfrp) = maybe [] (:[]) mdisk ++ (altShow seps ++ maybe "" altShow mfrp)
  altShow (PotentiallyInvalid fp) = fp


-- namespace-tail     = ( disk 1*"\" relative-path ; C:foo\bar is not valid
--                                                 ; namespaced paths are all absolute
--                      / disk *"\"
--                      / relative-path
--                      )
data NSTail = NST1 Char (NonEmpty Separator) RelFilePath
            | NST2 Char [Separator]
            | NST3 RelFilePath
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` NSTail)

instance AltShow NSTail where
  altShow (NST1 disk seps relfp) = disk:':':(altShow seps ++ altShow relfp)
  altShow (NST2 disk seps) = disk:':':altShow seps
  altShow (NST3 relfp) = altShow relfp


--  UNC = "\\" 1*pchar "\" 1*pchar  [ 1*"\" [ relative-path ] ]
data UNCShare = UNCShare Separator Separator
                         NonEmptyString
                         (NonEmpty Separator)
                         NonEmptyString
                         (Maybe (NonEmpty Separator, Maybe RelFilePath))
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` UNCShare)

instance AltShow UNCShare where
  altShow (UNCShare sep1 sep2 fp1 seps fp2 mrfp) = altShow sep1 ++ altShow sep2 ++ altShow fp1 ++ altShow seps ++ altShow fp2 ++ maybe "" (\(a, b) -> altShow a ++ maybe "" altShow b) mrfp

newtype NonEmptyString = NonEmptyString (NonEmpty Char)
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` NonEmptyString)

instance AltShow NonEmptyString where
  altShow (NonEmptyString ns) = NE.toList ns


-- | Windows API Namespaces
--
-- https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#namespaces
-- https://support.microsoft.com/en-us/topic/70b92942-a643-2f2d-2ac6-aad8acad49fb
-- https://superuser.com/a/1096784/854039
-- https://reverseengineering.stackexchange.com/a/15178
-- https://stackoverflow.com/a/25099634
--
-- namespace          = file-namespace / device-namespace / nt-namespace
-- file-namespace     = "\" "\" "?" "\"
-- device-namespace   = "\" "\" "." "\"
-- nt-namespace       = "\" "?" "?" "\"
data NameSpace = FileNameSpace
               | DeviceNameSpace
               | NTNameSpace
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` NameSpace)

instance AltShow NameSpace where
  altShow FileNameSpace = "\\\\?\\"
  altShow DeviceNameSpace = "\\\\.\\"
  altShow NTNameSpace = "\\??\\"


data Separator = UnixSep
               | WindowsSep
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` Separator)

instance AltShow Separator where
  altShow UnixSep = "/"
  altShow WindowsSep = "\\"

instance {-# OVERLAPS #-} AltShow (NonEmpty Separator) where
  altShow ne = mconcat $ NE.toList (altShow <$> ne)

instance {-# OVERLAPS #-} AltShow [Separator] where
  altShow [] = ""
  altShow ne = altShow (NE.fromList ne)

--  relative-path = 1*(path-name 1*"\") [ file-name ] / file-name
data RelFilePath = Rel1 (NonEmpty (NonEmptyString, NonEmpty Separator)) (Maybe FileName)
                 | Rel2 FileName
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` RelFilePath)

instance AltShow RelFilePath where
  altShow (Rel1 ns mf) = (mconcat $ NE.toList $ fmap (\(a, b) -> altShow a ++ altShow b) ns) ++ maybe "" altShow mf
  altShow (Rel2 fn) = altShow fn

--  file-name = 1*pchar [ stream ]
data FileName = FileName NonEmptyString (Maybe DataStream)
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` FileName)

instance AltShow FileName where
  altShow (FileName ns ds) = altShow ns ++ altShow ds

--  stream = ":" 1*schar [ ":" 1*schar ] / ":" ":" 1*schar
data DataStream = DS1 NonEmptyString (Maybe NonEmptyString)
                | DS2 NonEmptyString -- ::datatype
  deriving (GShow, Show, Eq, Ord, Generic)
  deriving Arbitrary via (GenericArbitraryU `AndShrinking` DataStream)

instance AltShow DataStream where
  altShow (DS1 ns Nothing) = ":" ++ altShow ns
  altShow (DS1 ns (Just ns2)) = ":" ++ altShow ns ++ ":" ++ altShow ns2
  altShow (DS2 ns) = "::" ++ altShow ns

instance Arbitrary WindowsFilePaths where
  arbitrary = scale (`mod` 20) $ genericArbitrary uniform

instance Arbitrary [Separator] where
  arbitrary = scale (`mod` 20) $ genericArbitrary uniform

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = scale (`mod` 20) $ do
    x <- arbitrary
    case x of
      [] -> (NE.fromList . (:[])) <$> arbitrary
      xs -> pure (NE.fromList xs)


main :: IO ()
main = defaultMain equivalentTests


equivalentTests :: TestTree
equivalentTests = testProperties "equivalence" $
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
    , property $ \(xs :: WindowsFilePaths)
      -> let p = (intercalate ";" (altShow <$> unWindowsFilePaths xs))
         in W.splitSearchPath p == LW.splitSearchPath p
    )
    ,
    ( "splitExtension (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.splitExtension p == LW.splitExtension p
    )
    ,
    ( "takeExtension (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.takeExtension p == LW.takeExtension p
    )
    ,
    ( "replaceExtension (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceExtension p s == LW.replaceExtension p s
    )
    ,
    ( "dropExtension (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.dropExtension p == LW.dropExtension p
    )
    ,
    ( "addExtension (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.addExtension p s == LW.addExtension p s
    )
    ,
    ( "hasExtension (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.hasExtension p == LW.hasExtension p
    )
    ,
    ( "splitExtensions (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.splitExtensions p == LW.splitExtensions p
    )
    ,
    ( "dropExtensions (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.dropExtensions p == LW.dropExtensions p
    )
    ,
    ( "takeExtensions (windows)"
    , property $ \p -> W.takeExtensions p == LW.takeExtensions p
    )
    ,
    ( "replaceExtensions (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceExtensions p s == LW.replaceExtensions p s
    )
    ,
    ( "isExtensionOf (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.isExtensionOf p s == LW.isExtensionOf p s
    )
    ,
    ( "stripExtension (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.stripExtension p s == LW.stripExtension p s
    )
    ,
    ( "splitFileName (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.splitFileName p == LW.splitFileName p
    )
    ,
    ( "takeFileName (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.takeFileName p == LW.takeFileName p
    )
    ,
    ( "replaceFileName (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceFileName p s == LW.replaceFileName p s
    )
    ,
    ( "dropFileName (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.dropFileName p == LW.dropFileName p
    )
    ,
    ( "takeBaseName (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.takeBaseName p == LW.takeBaseName p
    )
    ,
    ( "replaceBaseName (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceBaseName p s == LW.replaceBaseName p s
    )
    ,
    ( "takeDirectory (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.takeDirectory p == LW.takeDirectory p
    )
    ,
    ( "replaceDirectory (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.replaceDirectory p s == LW.replaceDirectory p s
    )
    ,
    ( "combine (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.combine p s == LW.combine p s
    )
    ,
    ( "splitPath (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.splitPath p == LW.splitPath p
    )
    ,
    ( "joinPath (windows)"
    , property $ \(xs :: WindowsFilePaths) ->
       let p = altShow <$> unWindowsFilePaths xs
       in W.joinPath p == LW.joinPath p
    )
    ,
    ( "splitDirectories (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.splitDirectories p == LW.splitDirectories p
    )
    ,
    ( "splitDrive (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.splitDrive p == LW.splitDrive p
    )
    ,
    ( "joinDrive (windows)"
    , property $ \(altShow @WindowsFilePath -> p) s -> W.joinDrive p s == LW.joinDrive p s
    )
    ,
    ( "takeDrive (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.takeDrive p == LW.takeDrive p
    )
    ,
    ( "hasDrive (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.hasDrive p == LW.hasDrive p
    )
    ,
    ( "dropDrive (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.dropDrive p == LW.dropDrive p
    )
    ,
    ( "isDrive (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.isDrive p == LW.isDrive p
    )
    ,
    ( "hasTrailingPathSeparator (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.hasTrailingPathSeparator p == LW.hasTrailingPathSeparator p
    )
    ,
    ( "addTrailingPathSeparator (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.addTrailingPathSeparator p == LW.addTrailingPathSeparator p
    )
    ,
    ( "dropTrailingPathSeparator (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> W.dropTrailingPathSeparator p == LW.dropTrailingPathSeparator p
    )
    ,
    ( "normalise (windows)"
    , property $ \(altShow @WindowsFilePath -> p) -> case p of
                         (l:':':rs)
                           -- new filepath normalises "a:////////" to "A:\\"
                           -- see https://github.com/haskell/filepath/commit/cb4890aa03a5ee61f16f7a08dd2d964fffffb385
                           | isAsciiLower l || isAsciiUpper l
                           , let (seps, path) = span LW.isPathSeparator rs
                           , length seps > 1 -> let np = l : ':' : LW.pathSeparator : path in W.normalise np == LW.normalise np
                         _ -> W.normalise p == LW.normalise p
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













