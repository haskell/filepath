-- This template expects CPP definitions for:
--     WINDOWS
--     POSIX
--     FILEPATH_NAME = PosixFilePath | WindowsFilePath  | AbstractFilePath
--     OSSTRING_NAME = PosixString   | WindowsString    | OsString
--     WORD_NAME     = PosixChar     | WindowsChar      | OsChar
--     WTOR          = PW            | WW               | OsChar
--     CTOR          = PS            | WS               | OsString

#ifdef WINDOWS
module System.AbstractFilePath.Windows
#elif defined(POSIX)
module System.AbstractFilePath.Posix
#else
module System.AbstractFilePath
#endif
  (
  -- * Types
#ifdef WINDOWS
    WindowsString
  , WindowsChar
  , WindowsFilePath
#elif defined(POSIX)
    PosixString
  , PosixChar
  , PosixFilePath
#else
    AbstractFilePath
  , OsString
  , OsChar
#endif
  -- * Filepath construction
#if defined(WINDOWS) || defined(POSIX)
  , toPlatformStringUtf
  , toPlatformStringEnc
  , toPlatformStringFS
  , pstr
  , packPlatformString
#else
  , toAbstractFilePathUtf
  , toAbstractFilePathEnc
  , toAbstractFilePathFS
  , afp
  , packAFP
#endif

  -- * Filepath deconstruction
#if defined(WINDOWS) || defined(POSIX)
  , fromPlatformStringUtf
  , fromPlatformStringEnc
  , fromPlatformStringFS
  , unpackPlatformString
#else
  , fromAbstractFilePathUtf
  , fromAbstractFilePathEnc
  , fromAbstractFilePathFS
  , unpackAFP
#endif

  -- * Word construction
  , unsafeFromChar

  -- * Word deconstruction
  , toChar

  -- * Separator predicates
  , pathSeparator
  , pathSeparators
  , isPathSeparator
  , searchPathSeparator
  , isSearchPathSeparator
  , extSeparator
  , isExtSeparator

  -- * $PATH methods
  , splitSearchPath,

  -- * Extension functions
    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    -- * Filename\/directory functions
    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    -- * Drive functions
    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    -- * Trailing slash functions
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    -- * File name manipulations
    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
  )
where


#ifdef WINDOWS
import System.AbstractFilePath.Types
import System.OsString.Windows
    ( unsafeFromChar
    , toChar
    , fromPlatformStringUtf
    , fromPlatformStringEnc
    , fromPlatformStringFS
    , packPlatformString
    , pstr
    , toPlatformStringUtf
    , toPlatformStringEnc
    , toPlatformStringFS
    , unpackPlatformString
    )
import Data.Bifunctor ( bimap )
import qualified System.AbstractFilePath.Windows.Internal as C

#elif defined(POSIX)

import System.AbstractFilePath.Types
import System.OsString.Posix
    ( unsafeFromChar
    , toChar
    , fromPlatformStringUtf
    , fromPlatformStringEnc
    , fromPlatformStringFS
    , packPlatformString
    , pstr
    , toPlatformStringUtf
    , toPlatformStringEnc
    , toPlatformStringFS
    , unpackPlatformString
    )
import Data.Bifunctor ( bimap )
import qualified System.AbstractFilePath.Posix.Internal as C

#else

import System.AbstractFilePath.Internal
    ( afp
    , fromAbstractFilePathUtf
    , fromAbstractFilePathEnc
    , fromAbstractFilePathFS
    , packAFP
    , toAbstractFilePathUtf
    , toAbstractFilePathEnc
    , toAbstractFilePathFS
    , unpackAFP
    )
import System.AbstractFilePath.Types
    ( AbstractFilePath )
import System.OsString

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.AbstractFilePath.Windows as C
#else
import qualified System.AbstractFilePath.Posix as C
#endif

import Data.Bifunctor
    ( bimap )
#endif
import System.OsString.Internal.Types


------------------------
-- Separator predicates


-- | The character that separates directories. In the case where more than
--   one character is possible, 'pathSeparator' is the \'ideal\' one.
--
-- > Windows: pathSeparator == '\\'
-- > Posix:   pathSeparator ==  '/'
-- > isPathSeparator pathSeparator
pathSeparator :: WORD_NAME
pathSeparator = WTOR C.pathSeparator

-- | The list of all possible separators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
pathSeparators :: [WORD_NAME]
pathSeparators = WTOR <$> C.pathSeparators

-- | Rather than using @(== 'pathSeparator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: WORD_NAME -> Bool
isPathSeparator (WTOR w) = C.isPathSeparator w

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
searchPathSeparator :: WORD_NAME
searchPathSeparator = WTOR C.searchPathSeparator

-- | Is the character a file separator?
--
-- > isSearchPathSeparator a == (a == searchPathSeparator)
isSearchPathSeparator :: WORD_NAME -> Bool
isSearchPathSeparator (WTOR w) = C.isSearchPathSeparator w


-- | File extension character
--
-- > extSeparator == '.'
extSeparator :: WORD_NAME
extSeparator = WTOR C.extSeparator


-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: WORD_NAME -> Bool
isExtSeparator (WTOR w) = C.isExtSeparator w


---------------------------------------------------------------------
-- Path methods (environment $PATH)

-- | Take a string, split it on the 'searchPathSeparator' character.
--   Blank items are ignored on Windows, and converted to @.@ on Posix.
--   On Windows path elements are stripped of quotes.
--
--   Follows the recommendations in
--   <http://www.opengroup.org/onlinepubs/009695399/basedefs/xbd_chap08.html>
--
-- > Posix:   splitSearchPath "File1:File2:File3"  == ["File1","File2","File3"]
-- > Posix:   splitSearchPath "File1::File2:File3" == ["File1",".","File2","File3"]
-- > Windows: splitSearchPath "File1;File2;File3"  == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;;File2;File3" == ["File1","File2","File3"]
-- > Windows: splitSearchPath "File1;\"File2\";File3" == ["File1","File2","File3"]
splitSearchPath :: OSSTRING_NAME -> [FILEPATH_NAME]
splitSearchPath (CTOR x) = fmap CTOR . C.splitSearchPath $ x



------------------------
-- Extension functions

-- | Split on the extension. 'addExtension' is the inverse.
--
-- > splitExtension "/directory/path.ext" == ("/directory/path",".ext")
-- > uncurry (<>) (splitExtension x) == x
-- > Valid x => uncurry addExtension (splitExtension x) == x
-- > splitExtension "file.txt" == ("file",".txt")
-- > splitExtension "file" == ("file","")
-- > splitExtension "file/file.txt" == ("file/file",".txt")
-- > splitExtension "file.txt/boris" == ("file.txt/boris","")
-- > splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext")
-- > splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred")
-- > splitExtension "file/path.txt/" == ("file/path.txt/","")
splitExtension :: FILEPATH_NAME -> (FILEPATH_NAME, OSSTRING_NAME)
splitExtension (CTOR x) = bimap CTOR CTOR $ C.splitExtension x


-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- > takeExtension "/directory/path.ext" == ".ext"
-- > takeExtension x == snd (splitExtension x)
-- > Valid x => takeExtension (addExtension x "ext") == ".ext"
-- > Valid x => takeExtension (replaceExtension x "ext") == ".ext"
takeExtension :: FILEPATH_NAME -> OSSTRING_NAME
takeExtension (CTOR x) = CTOR $ C.takeExtension x


-- | Remove the current extension and add another, equivalent to 'replaceExtension'.
--
-- > "/directory/path.txt" -<.> "ext" == "/directory/path.ext"
-- > "/directory/path.txt" -<.> ".ext" == "/directory/path.ext"
-- > "foo.o" -<.> "c" == "foo.c"
(-<.>) :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
(-<.>) = replaceExtension

-- | Set the extension of a file, overwriting one if already present, equivalent to '-<.>'.
--
-- > replaceExtension "/directory/path.txt" "ext" == "/directory/path.ext"
-- > replaceExtension "/directory/path.txt" ".ext" == "/directory/path.ext"
-- > replaceExtension "file.txt" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "bob" == "file.bob"
-- > replaceExtension "file" ".bob" == "file.bob"
-- > replaceExtension "file.txt" "" == "file"
-- > replaceExtension "file.fred.bob" "txt" == "file.fred.txt"
-- > replaceExtension x y == addExtension (dropExtension x) y
replaceExtension :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceExtension (CTOR path) (CTOR ext) = CTOR (C.replaceExtension path ext)


-- | Add an extension, even if there is already one there, equivalent to 'addExtension'.
--
-- > "/directory/path" <.> "ext" == "/directory/path.ext"
-- > "/directory/path" <.> ".ext" == "/directory/path.ext"
(<.>) :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
(<.>) = addExtension

-- | Remove last extension, and the \".\" preceding it.
--
-- > dropExtension "/directory/path.ext" == "/directory/path"
-- > dropExtension x == fst (splitExtension x)
dropExtension :: FILEPATH_NAME -> FILEPATH_NAME
dropExtension (CTOR x) = CTOR $ C.dropExtension x


-- | Add an extension, even if there is already one there, equivalent to '<.>'.
--
-- > addExtension "/directory/path" "ext" == "/directory/path.ext"
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > addExtension "/" "x" == "/.x"
-- > addExtension x "" == x
-- > Valid x => takeFileName (addExtension (addTrailingPathSeparator x) "ext") == ".ext"
-- > Windows: addExtension "\\\\share" ".txt" == "\\\\share\\.txt"
addExtension :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
addExtension (CTOR bs) (CTOR ext) = CTOR $ C.addExtension bs ext


-- | Does the given filename have an extension?
--
-- > hasExtension "/directory/path.ext" == True
-- > hasExtension "/directory/path" == False
-- > null (takeExtension x) == not (hasExtension x)
hasExtension :: FILEPATH_NAME -> Bool
hasExtension (CTOR x) = C.hasExtension x

-- | Does the given filename have the specified extension?
--
-- > "png" `isExtensionOf` "/directory/file.png" == True
-- > ".png" `isExtensionOf` "/directory/file.png" == True
-- > ".tar.gz" `isExtensionOf` "bar/foo.tar.gz" == True
-- > "ar.gz" `isExtensionOf` "bar/foo.tar.gz" == False
-- > "png" `isExtensionOf` "/directory/file.png.jpg" == False
-- > "csv/table.csv" `isExtensionOf` "/data/csv/table.csv" == False
isExtensionOf :: OSSTRING_NAME -> FILEPATH_NAME -> Bool
isExtensionOf (CTOR x) (CTOR y) = C.isExtensionOf x y

-- | Drop the given extension from a filepath, and the @\".\"@ preceding it.
--   Returns 'Nothing' if the filepath does not have the given extension, or
--   'Just' and the part before the extension if it does.
--
--   This function can be more predictable than 'dropExtensions', especially if the filename
--   might itself contain @.@ characters.
--
-- > stripExtension "hs.o" "foo.x.hs.o" == Just "foo.x"
-- > stripExtension "hi.o" "foo.x.hs.o" == Nothing
-- > dropExtension x == fromJust (stripExtension (takeExtension x) x)
-- > dropExtensions x == fromJust (stripExtension (takeExtensions x) x)
-- > stripExtension ".c.d" "a.b.c.d"  == Just "a.b"
-- > stripExtension ".c.d" "a.b..c.d" == Just "a.b."
-- > stripExtension "baz"  "foo.bar"  == Nothing
-- > stripExtension "bar"  "foobar"   == Nothing
-- > stripExtension ""     x          == Just x
stripExtension :: OSSTRING_NAME -> FILEPATH_NAME -> Maybe FILEPATH_NAME
stripExtension (CTOR bs) (CTOR x) = CTOR <$> C.stripExtension bs x

-- | Split on all extensions.
--
-- > splitExtensions "/directory/path.ext" == ("/directory/path",".ext")
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
-- > uncurry (<>) (splitExtensions x) == x
-- > Valid x => uncurry addExtension (splitExtensions x) == x
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: FILEPATH_NAME -> (FILEPATH_NAME, OSSTRING_NAME)
splitExtensions (CTOR x) = bimap CTOR CTOR $ C.splitExtensions x


-- | Drop all extensions.
--
-- > dropExtensions "/directory/path.ext" == "/directory/path"
-- > dropExtensions "file.tar.gz" == "file"
-- > not $ hasExtension $ dropExtensions x
-- > not $ any isExtSeparator $ takeFileName $ dropExtensions x
dropExtensions :: FILEPATH_NAME -> FILEPATH_NAME
dropExtensions (CTOR x) = CTOR $ C.dropExtensions x


-- | Get all extensions.
--
-- > takeExtensions "/directory/path.ext" == ".ext"
-- > takeExtensions "file.tar.gz" == ".tar.gz"
takeExtensions :: FILEPATH_NAME -> OSSTRING_NAME
takeExtensions (CTOR x) = CTOR $ C.takeExtensions x

-- | Replace all extensions of a file with a new extension. Note
--   that 'replaceExtension' and 'addExtension' both work for adding
--   multiple extensions, so only required when you need to drop
--   all extensions first.
--
-- > replaceExtensions "file.fred.bob" "txt" == "file.txt"
-- > replaceExtensions "file.fred.bob" "tar.gz" == "file.tar.gz"
replaceExtensions :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceExtensions (CTOR x) (CTOR y) = CTOR $ C.replaceExtensions x y


------------------------
-- Drive functions

-- | Split a path into a drive and a path.
--   On Posix, \/ is a Drive.
--
-- > uncurry (<>) (splitDrive x) == x
-- > Windows: splitDrive "file" == ("","file")
-- > Windows: splitDrive "c:/file" == ("c:/","file")
-- > Windows: splitDrive "c:\\file" == ("c:\\","file")
-- > Windows: splitDrive "\\\\shared\\test" == ("\\\\shared\\","test")
-- > Windows: splitDrive "\\\\shared" == ("\\\\shared","")
-- > Windows: splitDrive "\\\\?\\UNC\\shared\\file" == ("\\\\?\\UNC\\shared\\","file")
-- > Windows: splitDrive "\\\\?\\UNCshared\\file" == ("\\\\?\\","UNCshared\\file")
-- > Windows: splitDrive "\\\\?\\d:\\file" == ("\\\\?\\d:\\","file")
-- > Windows: splitDrive "/d" == ("","/d")
-- > Posix:   splitDrive "/test" == ("/","test")
-- > Posix:   splitDrive "//test" == ("//","test")
-- > Posix:   splitDrive "test/file" == ("","test/file")
-- > Posix:   splitDrive "file" == ("","file")
splitDrive :: FILEPATH_NAME -> (FILEPATH_NAME, FILEPATH_NAME)
splitDrive (CTOR p) = bimap CTOR CTOR $ C.splitDrive p


-- | Join a drive and the rest of the path.
--
-- > Valid x => uncurry joinDrive (splitDrive x) == x
-- > Windows: joinDrive "C:" "foo" == "C:foo"
-- > Windows: joinDrive "C:\\" "bar" == "C:\\bar"
-- > Windows: joinDrive "\\\\share" "foo" == "\\\\share\\foo"
-- > Windows: joinDrive "/:" "foo" == "/:\\foo"
joinDrive :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
joinDrive (CTOR a) (CTOR b) = CTOR $ C.joinDrive a b


-- | Get the drive from a filepath.
--
-- > takeDrive x == fst (splitDrive x)
takeDrive :: FILEPATH_NAME -> FILEPATH_NAME
takeDrive (CTOR x) = CTOR $ C.takeDrive x


-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FILEPATH_NAME -> FILEPATH_NAME
dropDrive (CTOR x) = CTOR $ C.dropDrive x


-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (takeDrive x)
-- > Posix:   hasDrive "/foo" == True
-- > Windows: hasDrive "C:\\foo" == True
-- > Windows: hasDrive "C:foo" == True
-- >          hasDrive "foo" == False
-- >          hasDrive "" == False
hasDrive :: FILEPATH_NAME -> Bool
hasDrive (CTOR x) = C.hasDrive x



-- | Is an element a drive
--
-- > Posix:   isDrive "/" == True
-- > Posix:   isDrive "/foo" == False
-- > Windows: isDrive "C:\\" == True
-- > Windows: isDrive "C:\\foo" == False
-- >          isDrive "" == False
isDrive :: FILEPATH_NAME -> Bool
isDrive (CTOR x) = C.isDrive x


---------------------------------------------------------------------
-- Operations on a filepath, as a list of directories

-- | Split a filename into directory and file. '</>' is the inverse.
--   The first component will often end with a trailing slash.
--
-- > splitFileName "/directory/file.ext" == ("/directory/","file.ext")
-- > Valid x => uncurry (</>) (splitFileName x) == x || fst (splitFileName x) == "./"
-- > Valid x => isValid (fst (splitFileName x))
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("./", "bob")
-- > Posix:   splitFileName "/" == ("/","")
-- > Windows: splitFileName "c:" == ("c:","")
splitFileName :: FILEPATH_NAME -> (FILEPATH_NAME, FILEPATH_NAME)
splitFileName (CTOR x) = bimap CTOR CTOR $ C.splitFileName x


-- | Set the filename.
--
-- > replaceFileName "/directory/other.txt" "file.ext" == "/directory/file.ext"
-- > Valid x => replaceFileName x (takeFileName x) == x
replaceFileName :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceFileName (CTOR x) (CTOR y) = CTOR $ C.replaceFileName x y


-- | Drop the filename. Unlike 'takeDirectory', this function will leave
--   a trailing path separator on the directory.
--
-- > dropFileName "/directory/file.ext" == "/directory/"
-- > dropFileName x == fst (splitFileName x)
dropFileName :: FILEPATH_NAME -> FILEPATH_NAME
dropFileName (CTOR x) = CTOR $ C.dropFileName x


-- | Get the file name.
--
-- > takeFileName "/directory/file.ext" == "file.ext"
-- > takeFileName "test/" == ""
-- > takeFileName x `isSuffixOf` x
-- > takeFileName x == snd (splitFileName x)
-- > Valid x => takeFileName (replaceFileName x "fred") == "fred"
-- > Valid x => takeFileName (x </> "fred") == "fred"
-- > Valid x => isRelative (takeFileName x)
takeFileName :: FILEPATH_NAME -> FILEPATH_NAME
takeFileName (CTOR x) = CTOR $ C.takeFileName x


-- | Get the base name, without an extension or path.
--
-- > takeBaseName "/directory/file.ext" == "file"
-- > takeBaseName "file/test.txt" == "test"
-- > takeBaseName "dave.ext" == "dave"
-- > takeBaseName "" == ""
-- > takeBaseName "test" == "test"
-- > takeBaseName (addTrailingPathSeparator x) == ""
-- > takeBaseName "file/file.tar.gz" == "file.tar"
takeBaseName :: FILEPATH_NAME -> FILEPATH_NAME
takeBaseName (CTOR x) = CTOR $ C.takeBaseName x


-- | Set the base name.
--
-- > replaceBaseName "/directory/other.ext" "file" == "/directory/file.ext"
-- > replaceBaseName "file/test.txt" "bob" == "file/bob.txt"
-- > replaceBaseName "fred" "bill" == "bill"
-- > replaceBaseName "/dave/fred/bob.gz.tar" "new" == "/dave/fred/new.tar"
-- > Valid x => replaceBaseName x (takeBaseName x) == x
replaceBaseName :: FILEPATH_NAME -> OSSTRING_NAME -> FILEPATH_NAME
replaceBaseName (CTOR path) (CTOR name) = CTOR $ C.replaceBaseName path name


-- | Is an item either a directory or the last character a path separator?
--
-- > hasTrailingPathSeparator "test" == False
-- > hasTrailingPathSeparator "test/" == True
hasTrailingPathSeparator :: FILEPATH_NAME -> Bool
hasTrailingPathSeparator (CTOR x) = C.hasTrailingPathSeparator x


-- | Add a trailing file path separator if one is not already present.
--
-- > hasTrailingPathSeparator (addTrailingPathSeparator x)
-- > hasTrailingPathSeparator x ==> addTrailingPathSeparator x == x
-- > Posix:    addTrailingPathSeparator "test/rest" == "test/rest/"
addTrailingPathSeparator :: FILEPATH_NAME -> FILEPATH_NAME
addTrailingPathSeparator (CTOR bs) = CTOR $ C.addTrailingPathSeparator bs


-- | Remove any trailing path separators
--
-- > dropTrailingPathSeparator "file/test/" == "file/test"
-- >           dropTrailingPathSeparator "/" == "/"
-- > Windows:  dropTrailingPathSeparator "\\" == "\\"
-- > Posix:    not (hasTrailingPathSeparator (dropTrailingPathSeparator x)) || isDrive x
dropTrailingPathSeparator :: FILEPATH_NAME -> FILEPATH_NAME
dropTrailingPathSeparator (CTOR x) = CTOR $ C.dropTrailingPathSeparator x


-- | Get the directory name, move up one level.
--
-- >           takeDirectory "/directory/other.ext" == "/directory"
-- >           takeDirectory x `isPrefixOf` x || takeDirectory x == "."
-- >           takeDirectory "foo" == "."
-- >           takeDirectory "/" == "/"
-- >           takeDirectory "/foo" == "/"
-- >           takeDirectory "/foo/bar/baz" == "/foo/bar"
-- >           takeDirectory "/foo/bar/baz/" == "/foo/bar/baz"
-- >           takeDirectory "foo/bar/baz" == "foo/bar"
-- > Windows:  takeDirectory "foo\\bar" == "foo"
-- > Windows:  takeDirectory "foo\\bar\\\\" == "foo\\bar"
-- > Windows:  takeDirectory "C:\\" == "C:\\"
takeDirectory :: FILEPATH_NAME -> FILEPATH_NAME
takeDirectory (CTOR x) = CTOR $ C.takeDirectory x


-- | Set the directory, keeping the filename the same.
--
-- > replaceDirectory "root/file.ext" "/directory/" == "/directory/file.ext"
-- > Valid x => replaceDirectory x (takeDirectory x) `equalFilePath` x
replaceDirectory :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
replaceDirectory (CTOR file) (CTOR dir) = CTOR $ C.replaceDirectory file dir


-- | An alias for '</>'.
combine :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
combine (CTOR a) (CTOR b) = CTOR $ C.combine a b


-- | Combine two paths with a path separator.
--   If the second path starts with a path separator or a drive letter, then it returns the second.
--   The intention is that @readFile (dir '</>' file)@ will access the same file as
--   @setCurrentDirectory dir; readFile file@.
--
-- > Posix:   "/directory" </> "file.ext" == "/directory/file.ext"
-- > Windows: "/directory" </> "file.ext" == "/directory\\file.ext"
-- >          "directory" </> "/file.ext" == "/file.ext"
-- > Valid x => (takeDirectory x </> takeFileName x) `equalFilePath` x
--
--   Combined:
--
-- > Posix:   "/" </> "test" == "/test"
-- > Posix:   "home" </> "bob" == "home/bob"
-- > Posix:   "x:" </> "foo" == "x:/foo"
-- > Windows: "C:\\foo" </> "bar" == "C:\\foo\\bar"
-- > Windows: "home" </> "bob" == "home\\bob"
--
--   Not combined:
--
-- > Posix:   "home" </> "/bob" == "/bob"
-- > Windows: "home" </> "C:\\bob" == "C:\\bob"
--
--   Not combined (tricky):
--
--   On Windows, if a filepath starts with a single slash, it is relative to the
--   root of the current drive. In [1], this is (confusingly) referred to as an
--   absolute path.
--   The current behavior of '</>' is to never combine these forms.
--
-- > Windows: "home" </> "/bob" == "/bob"
-- > Windows: "home" </> "\\bob" == "\\bob"
-- > Windows: "C:\\home" </> "\\bob" == "\\bob"
--
--   On Windows, from [1]: "If a file name begins with only a disk designator
--   but not the backslash after the colon, it is interpreted as a relative path
--   to the current directory on the drive with the specified letter."
--   The current behavior of '</>' is to never combine these forms.
--
-- > Windows: "D:\\foo" </> "C:bar" == "C:bar"
-- > Windows: "C:\\foo" </> "C:bar" == "C:bar"
(</>) :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
(</>) = combine


-- | Split a path by the directory separator.
--
-- > splitPath "/directory/file.ext" == ["/","directory/","file.ext"]
-- > concat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > Windows: splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
-- > Posix:   splitPath "/file/test" == ["/","file/","test"]
splitPath :: FILEPATH_NAME -> [FILEPATH_NAME]
splitPath (CTOR bs) = CTOR <$> C.splitPath bs

-- | Just as 'splitPath', but don't add the trailing slashes to each element.
--
-- >          splitDirectories "/directory/file.ext" == ["/","directory","file.ext"]
-- >          splitDirectories "test/file" == ["test","file"]
-- >          splitDirectories "/test/file" == ["/","test","file"]
-- > Windows: splitDirectories "C:\\test\\file" == ["C:\\", "test", "file"]
-- >          Valid x => joinPath (splitDirectories x) `equalFilePath` x
-- >          splitDirectories "" == []
-- > Windows: splitDirectories "C:\\test\\\\\\file" == ["C:\\", "test", "file"]
-- >          splitDirectories "/test///file" == ["/","test","file"]
splitDirectories :: FILEPATH_NAME -> [FILEPATH_NAME]
splitDirectories (CTOR x) = CTOR <$> C.splitDirectories x

-- | Join path elements back together.
--
-- > joinPath z == foldr (</>) "" z
-- > joinPath ["/","directory/","file.ext"] == "/directory/file.ext"
-- > Valid x => joinPath (splitPath x) == x
-- > joinPath [] == ""
-- > Posix: joinPath ["test","file","path"] == "test/file/path"
joinPath :: [FILEPATH_NAME] -> FILEPATH_NAME
joinPath = foldr (</>) (CTOR mempty)









------------------------
-- File name manipulations


-- | Equality of two filepaths.
--   If you call @System.Directory.canonicalizePath@
--   first this has a much better chance of working.
--   Note that this doesn't follow symlinks or DOSNAM~1s.
--
-- Similar to 'normalise', this does not expand @".."@, because of symlinks.
--
-- >          x == y ==> equalFilePath x y
-- >          normalise x == normalise y ==> equalFilePath x y
-- >          equalFilePath "foo" "foo/"
-- >          not (equalFilePath "/a/../c" "/c")
-- >          not (equalFilePath "foo" "/foo")
-- > Posix:   not (equalFilePath "foo" "FOO")
-- > Windows: equalFilePath "foo" "FOO"
-- > Windows: not (equalFilePath "C:" "C:/")
equalFilePath :: FILEPATH_NAME -> FILEPATH_NAME -> Bool
equalFilePath (CTOR p1) (CTOR p2) = C.equalFilePath p1 p2

-- | Contract a filename, based on a relative path. Note that the resulting path
--   will never introduce @..@ paths, as the presence of symlinks means @..\/b@
--   may not reach @a\/b@ if it starts from @a\/c@. For a worked example see
--   <http://neilmitchell.blogspot.co.uk/2015/10/filepaths-are-subtle-symlinks-are-hard.html this blog post>.
--
--   The corresponding @makeAbsolute@ function can be found in
--   @System.Directory@.
--
-- >          makeRelative "/directory" "/directory/file.ext" == "file.ext"
-- >          Valid x => makeRelative (takeDirectory x) x `equalFilePath` takeFileName x
-- >          makeRelative x x == "."
-- >          Valid x y => equalFilePath x y || (isRelative x && makeRelative y x == x) || equalFilePath (y </> makeRelative y x) x
-- > Windows: makeRelative "C:\\Home" "c:\\home\\bob" == "bob"
-- > Windows: makeRelative "C:\\Home" "c:/home/bob" == "bob"
-- > Windows: makeRelative "C:\\Home" "D:\\Home\\Bob" == "D:\\Home\\Bob"
-- > Windows: makeRelative "C:\\Home" "C:Home\\Bob" == "C:Home\\Bob"
-- > Windows: makeRelative "/Home" "/home/bob" == "bob"
-- > Windows: makeRelative "/" "//" == "//"
-- > Posix:   makeRelative "/Home" "/home/bob" == "/home/bob"
-- > Posix:   makeRelative "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
-- > Posix:   makeRelative "/fred" "bob" == "bob"
-- > Posix:   makeRelative "/file/test" "/file/test/fred" == "fred"
-- > Posix:   makeRelative "/file/test" "/file/test/fred/" == "fred/"
-- > Posix:   makeRelative "some/path" "some/path/a/b/c" == "a/b/c"
makeRelative :: FILEPATH_NAME -> FILEPATH_NAME -> FILEPATH_NAME
makeRelative (CTOR root) (CTOR path) = CTOR $ C.makeRelative root path

-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- Does not remove @".."@, because of symlinks.
--
-- > Posix:   normalise "/file/\\test////" == "/file/\\test/"
-- > Posix:   normalise "/file/./test" == "/file/test"
-- > Posix:   normalise "/test/file/../bob/fred/" == "/test/file/../bob/fred/"
-- > Posix:   normalise "../bob/fred/" == "../bob/fred/"
-- > Posix:   normalise "/a/../c" == "/a/../c"
-- > Posix:   normalise "./bob/fred/" == "bob/fred/"
-- > Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > Windows: normalise "c:\\" == "C:\\"
-- > Windows: normalise "C:.\\" == "C:"
-- > Windows: normalise "\\\\server\\test" == "\\\\server\\test"
-- > Windows: normalise "//server/test" == "\\\\server\\test"
-- > Windows: normalise "c:/file" == "C:\\file"
-- > Windows: normalise "/file" == "\\file"
-- > Windows: normalise "\\" == "\\"
-- > Windows: normalise "/./" == "\\"
-- >          normalise "." == "."
-- > Posix:   normalise "./" == "./"
-- > Posix:   normalise "./." == "./"
-- > Posix:   normalise "/./" == "/"
-- > Posix:   normalise "/" == "/"
-- > Posix:   normalise "bob/fred/." == "bob/fred/"
-- > Posix:   normalise "//home" == "/home"
normalise :: FILEPATH_NAME -> FILEPATH_NAME
normalise (CTOR filepath) = CTOR $ C.normalise filepath


-- | Is a filepath valid, i.e. could you create a file like it? This function checks for invalid names,
--   and invalid characters, but does not check if length limits are exceeded, as these are typically
--   filesystem dependent.
--
-- >          isValid "" == False
-- >          isValid "\0" == False
-- > Posix:   isValid "/random_ path:*" == True
-- > Posix:   isValid x == not (null x)
-- > Windows: isValid "c:\\test" == True
-- > Windows: isValid "c:\\test:of_test" == False
-- > Windows: isValid "test*" == False
-- > Windows: isValid "c:\\test\\nul" == False
-- > Windows: isValid "c:\\test\\prn.txt" == False
-- > Windows: isValid "c:\\nul\\file" == False
-- > Windows: isValid "\\\\" == False
-- > Windows: isValid "\\\\\\foo" == False
-- > Windows: isValid "\\\\?\\D:file" == False
-- > Windows: isValid "foo\tbar" == False
-- > Windows: isValid "nul .txt" == False
-- > Windows: isValid " nul.txt" == True
isValid :: FILEPATH_NAME -> Bool
isValid (CTOR filepath) = C.isValid filepath


-- | Take a filepath and make it valid; does not change already valid filepaths.
--
-- > isValid (makeValid x)
-- > isValid x ==> makeValid x == x
-- > makeValid "" == "_"
-- > makeValid "file\0name" == "file_name"
-- > Windows: makeValid "c:\\already\\/valid" == "c:\\already\\/valid"
-- > Windows: makeValid "c:\\test:of_test" == "c:\\test_of_test"
-- > Windows: makeValid "test*" == "test_"
-- > Windows: makeValid "c:\\test\\nul" == "c:\\test\\nul_"
-- > Windows: makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
-- > Windows: makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
-- > Windows: makeValid "c:\\nul\\file" == "c:\\nul_\\file"
-- > Windows: makeValid "\\\\\\foo" == "\\\\drive"
-- > Windows: makeValid "\\\\?\\D:file" == "\\\\?\\D:\\file"
-- > Windows: makeValid "nul .txt" == "nul _.txt"
makeValid :: FILEPATH_NAME -> FILEPATH_NAME
makeValid (CTOR path) = CTOR $ C.makeValid path


-- | Is a path relative, or is it fixed to the root?
--
-- > Windows: isRelative "path\\test" == True
-- > Windows: isRelative "c:\\test" == False
-- > Windows: isRelative "c:test" == True
-- > Windows: isRelative "c:\\" == False
-- > Windows: isRelative "c:/" == False
-- > Windows: isRelative "c:" == True
-- > Windows: isRelative "\\\\foo" == False
-- > Windows: isRelative "\\\\?\\foo" == False
-- > Windows: isRelative "\\\\?\\UNC\\foo" == False
-- > Windows: isRelative "/foo" == True
-- > Windows: isRelative "\\foo" == True
-- > Posix:   isRelative "test/path" == True
-- > Posix:   isRelative "/test" == False
-- > Posix:   isRelative "/" == False
--
-- According to [1]:
--
-- * "A UNC name of any format [is never relative]."
--
-- * "You cannot use the "\\?\" prefix with a relative path."
isRelative :: FILEPATH_NAME -> Bool
isRelative (CTOR x) = C.isRelative x


-- | @not . 'isRelative'@
--
-- > isAbsolute x == not (isRelative x)
isAbsolute :: FILEPATH_NAME -> Bool
isAbsolute (CTOR x) = C.isAbsolute x
