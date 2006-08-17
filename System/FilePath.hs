
{- |
Module      :  System.FilePath
Copyright   :  (c) Neil Mitchell 2005-2006
License     :  BSD3

Maintainer  :  http://www.cs.york.ac.uk/~ndm/
Stability   :  in-progress
Portability :  portable

A library for FilePath manipulations, designed to be cross platform.
This library will select the correct type of FilePath's for the
platform the code is running on at runtime. For more details see 
<http://www.cs.york.ac.uk/~ndm/projects/libraries.php>

Some short examples:

You are given a C file, you want to figure out the corresponding object (.o) file:

@'setExtension' file \"o\"@

Haskell module Main imports Test, you have the file named main:

@['setFileName' path_to_main \"Test\" '<.>' ext | ext <- [\"hs\",\"lhs\"] ]@

You want to download a file from the web and save it to disk:

@do let file = 'makeValid' url
   'ensureDirectory' ('getDirectory' file)@

You want to compile a Haskell file, but put the hi file under \"interface\"

@'getDirectory' file '</>' \"interface\" '</>' ('getFileName' file \`setExtension\` \"hi\"@)

You want to display a filename to the user, as neatly as possible

@'shortPath' file >>= putStrLn@

The examples in code format descibed by each function are used to generate
tests, and should give clear semantics for the functions.
-}

module System.FilePath
    (
    -- * The basic functions
    FilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    fileSeparator, isFileSeparator,
    extSeparator, isExtSeparator,
    
    -- * Path methods (environment $PATH)
    splitFiles, getPath,
    
    -- * Extension methods
    splitExtension, joinExtension,
    getExtension, setExtension, dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, getExtensions,
    
    {- DRIVE_SECTION
    -- * Drive methods
    splitDrive, joinDrive,
    getDrive, setDrive, hasDrive, dropDrive,
    END_DRIVE_SECTION -}
    
    -- * Operations on a filepath, as a list of directories
    splitFileName, joinFileName,
    getFileName, setFileName, dropFileName, addFileName,
    getBaseName, setBaseName,
    getDirectory, setDirectory, isDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,
    
    -- * File name manipulators
    canonicalPath,
    normalise, equalFilePath,
    fullPath, fullPathWith, shortPath, shortPathWith,
    isRelative, isAbsolute,
    isValid, makeValid,
    
    -- * Directory operations
    getDirectoryList, ensureDirectory,
    
    -- * Temporary file operations
    getTemporaryFile, getTemporaryFileNew, getTemporaryFileSeed
    )
    where

import Data.Maybe(isJust, fromMaybe, fromJust)
import Data.Char(toLower, toUpper)
import Data.List(isPrefixOf, inits)
import Control.Monad(when, filterM)

import System.Info(os, compilerName)

import System.Environment(getEnv, getProgName)
import System.Directory(getCurrentDirectory, doesFileExist, doesDirectoryExist,
                        getTemporaryDirectory, getDirectoryContents, createDirectory)


infixr 7  <.>
infixr 5  </>





---------------------------------------------------------------------
-- Platform Abstraction Methods (private)

data Force = ForcePosix
           | ForceNone
           | ForceWindows
           deriving Eq

forceEffectView = let forceEffect = ForceNone
                  in forceEffect


-- | What is the name of the OS? The real name, Hugs and GHC get this wrong...
osName :: String
osName = if compilerName == "yhc" || os /= "mingw32"
         then os
         else "windows"


-- | Is the operating system Unix or Linux like
isPosix :: Bool
isPosix = not isWindows && forceEffectView /= ForceWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = osName == "windows" && forceEffectView /= ForcePosix





---------------------------------------------------------------------
-- The basic functions

-- | The character that seperates directories. In the case where more than
--   one character is possible, 'pathSeperator' is the 'ideal' one.
--
-- > Windows: pathSeparator == '\\'
-- > Posix:   pathSeparator ==  '/'
-- > isPathSeparator pathSeparator
pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/'

-- | The list of all possible seperators.
--
-- > Windows: pathSeparators == ['\\', '/']
-- > Posix:   pathSeparators == ['/']
-- > pathSeparator `elem` pathSeparators
pathSeparators :: [Char]
pathSeparators = if isWindows then "\\/" else "/"

-- | Rather than using @(== 'pathSeperator')@, use this. Test if something
--   is a path separator.
--
-- > isPathSeparator a == (a `elem` pathSeparators)
isPathSeparator :: Char -> Bool
isPathSeparator = (`elem` pathSeparators)


-- | A list of possible file separators, between the $PATH variable
--
-- > Windows: fileSeparator == ';'
-- > Posix:   fileSeparator == ':'
fileSeparator :: Char
fileSeparator = if isWindows then ';' else ':'

-- | Is the character a file separator?
--
-- > isFileSeparator a == (a == fileSeparator)
isFileSeparator :: Char -> Bool
isFileSeparator = (== fileSeparator)


-- | File extension character
--
-- > extSeparator == '.'
extSeparator :: Char
extSeparator = '.'

-- | Is the character an extension character?
--
-- > isExtSeparator a == (a == extSeparator)
isExtSeparator :: Char -> Bool
isExtSeparator = (== extSeparator)




---------------------------------------------------------------------
-- Path methods (environment $PATH)

-- | Take a string, split it on the 'fileSeparators' character.
--
-- > Windows: splitFiles "File1;File2;File3" == ["File1","File2","File3"]
-- > Posix:   splitFiles "File1:File2:File3" == ["File1","File2","File3"]
splitFiles :: String -> [FilePath]
splitFiles = f
    where
    f xs = case break isFileSeparator xs of
           ([],  [])   -> []
           ([],  post) -> f (tail post)
           (pre, [])   -> [pre]
           (pre, post) -> pre : f (tail post)

-- | Get a list of filepaths in the $PATH.
getPath :: IO [FilePath]
getPath = fmap splitFiles (getEnv "PATH")


---------------------------------------------------------------------
-- Extension methods

-- | Split on the extension.
--
-- > uncurry (++) (splitExtension x) == x
-- > splitExtension "file.txt" == ("file",".txt")
-- > splitExtension "file" == ("file","")
-- > splitExtension "file/file.txt" == ("file/file",".txt")
-- > splitExtension "file.txt/boris" == ("file.txt/boris","")
-- > splitExtension "file.txt/boris.ext" == ("file.txt/boris",".ext")
-- > splitExtension "file/path.txt.bob.fred" == ("file/path.txt.bob",".fred")
-- > splitExtension "file/path.txt/" == ("file/path.txt/","")
splitExtension :: FilePath -> (String, String)
splitExtension x = case d of
                       "" -> (x,"")
                       (y:ys) -> (a ++ reverse ys, y : reverse c)
    where
        (a,b) = splitFileName x
        (c,d) = break isExtSeparator $ reverse b

-- | Join an extension and a filepath.
--
-- > uncurry joinExtension (splitExtension x) == x
joinExtension :: String -> String -> FilePath
joinExtension = addExtension

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise.
--
-- > getExtension x == snd (splitExtension x)
-- > getExtension (addExtension x "ext") == ".ext"
-- > getExtension (setExtension x "ext") == ".ext"
getExtension :: FilePath -> String
getExtension = snd . splitExtension

-- | Set the extension of a file, overwriting one if already present.
--
-- > setExtension "file.txt" ".bob" == "file.bob"
-- > setExtension "file.txt" "bob" == "file.bob"
-- > setExtension "file" ".bob" == "file.bob"
-- > setExtension "file.txt" "" == "file"
-- > setExtension "file.fred.bob" "txt" == "file.fred.txt"
setExtension :: FilePath -> String -> FilePath
setExtension x y = joinExtension a y
    where (a,b) = splitExtension x

-- | Alias to 'addExtension', for people who like that sort of thing.
(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

-- | Remove last extension, and any . following it.
--
-- > dropExtension x == fst (splitExtension x)
dropExtension :: FilePath -> FilePath
dropExtension = fst . splitExtension

-- | Add an extension, even if there is already one there. 
--   E.g. @addExtension \"foo.txt\" \"bat\" -> \"foo.txt.bat\"@.
--
-- > addExtension "file.txt" "bib" == "file.txt.bib"
-- > addExtension "file." ".bib" == "file..bib"
-- > addExtension "file" ".bib" == "file.bib"
-- > Windows: addExtension "\\\\share" ".txt" == "\\\\share\\.txt"
addExtension :: FilePath -> String -> FilePath
addExtension file "" = file
addExtension file xs@(x:_) = joinDrive a res
    where 
        res = if isExtSeparator x then b ++ xs
              else b ++ [extSeparator] ++ xs
                
        (a,b) = splitDrive file

-- | Does the given filename have an extension?
--
-- > null (getExtension x) == not (hasExtension x)
hasExtension :: FilePath -> Bool
hasExtension = any isExtSeparator . getFileName


-- | Split on all extensions
--
-- > splitExtensions "file.tar.gz" == ("file",".tar.gz")
splitExtensions :: FilePath -> (FilePath, String)
splitExtensions x = (a ++ c, d)
    where
        (a,b) = splitFileName x
        (c,d) = break isExtSeparator b

-- | Drop all extensions
--
-- > not $ hasExtension (dropExtensions x)
dropExtensions :: FilePath -> FilePath
dropExtensions = fst . splitExtensions

-- | Get all extensions
getExtensions :: FilePath -> String
getExtensions = snd . splitExtensions



---------------------------------------------------------------------
-- Drive methods

-- | Is the given character a valid drive letter?
-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: Char -> Bool
isLetter x | x >= 'a' && x <= 'z' = True
           | x >= 'A' && x <= 'Z' = True
           | otherwise = False

-- | Split a path into a drive and a path.
--   On Unix, \/ is a Drive.
--
-- > uncurry (++) (splitDrive x) == x
-- > Windows: splitDrive "file" == ("","file")
-- > Windows: splitDrive "c:/file" == ("c:/","file")
-- > Windows: splitDrive "c:\\file" == ("c:\\","file")
-- > Windows: splitDrive "\\\\shared\\test" == ("\\\\shared\\","test")
-- > Windows: splitDrive "\\\\shared" == ("\\\\shared","")
-- > Windows: splitDrive "\\\\?\\UNC\\shared\\file" == ("\\\\?\\UNC\\shared\\","file")
-- > Windows: splitDrive "\\\\?\\d:\\file" == ("\\\\?\\d:\\","file")
-- > Windows: splitDrive "/d" == ("/","d")
-- > Posix:   splitDrive "/test" == ("/","test")
-- > Posix:   splitDrive "test/file" == ("","test/file")
-- > Posix:   splitDrive "file" == ("","file")
splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive x | isPosix = case x of
                             '/':xs -> ("/",xs)
                             xs -> ("",xs)

splitDrive x | isJust y = fromJust y
    where y = readDriveLetter x

splitDrive x | isJust y = fromJust y
    where y = readDriveUNC x

splitDrive x | isJust y = fromJust y
    where y = readDriveShare x

splitDrive (x:xs) | isPathSeparator x = addSlash [x] xs

splitDrive x = ("",x)

addSlash a xs = (a++c,d)
    where (c,d) = span isPathSeparator xs

-- http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp
-- "\\?\D:\<path>" or "\\?\UNC\<server>\<share>"
-- a is "\\?\"
readDriveUNC :: FilePath -> Maybe (FilePath, FilePath)
readDriveUNC (s1:s2:'?':s3:xs) | all isPathSeparator [s1,s2,s3] =
    case map toUpper xs of
        ('U':'N':'C':s4:_) | isPathSeparator s4 ->
            let (a,b) = readDriveShareName (drop 4 xs)
            in Just (s1:s2:'?':s3:take 4 xs ++ a, b)
        _ -> case readDriveLetter xs of
                 Just (a,b) -> Just (s1:s2:'?':s3:a,b)
                 Nothing -> Nothing
readDriveUNC x = Nothing

-- c:\
readDriveLetter :: String -> Maybe (FilePath, FilePath)
readDriveLetter [x,':'] | isLetter x = Just ([x,':'],"")
readDriveLetter (x:':':y:xs) | isLetter x && isPathSeparator y = Just $ addSlash [x,':'] (y:xs)
readDriveLetter x = Nothing

-- \\sharename\
readDriveShare :: String -> Maybe (FilePath, FilePath)
readDriveShare (s1:s2:xs) | isPathSeparator s1 && isPathSeparator s2 =
        Just (s1:s2:a,b)
    where (a,b) = readDriveShareName xs
readDriveShare x = Nothing

-- assume you have already seen \\
-- share\bob -> "share","\","bob"
readDriveShareName :: String -> (FilePath, FilePath)
readDriveShareName name = addSlash a b
    where (a,b) = break isPathSeparator name
    


-- | Join a drive and the rest of the path.
--
-- > uncurry joinDrive (splitDrive x) == x
joinDrive :: FilePath -> FilePath -> FilePath
joinDrive a b | isPosix = a ++ b
              | null a = b
              | null b = a
              | isPathSeparator (last a) = a ++ b
              | otherwise = a ++ [pathSeparator] ++ b

-- | Set the drive, from the filepath.
--
-- > setDrive x (getDrive x) == x
setDrive :: FilePath -> String -> FilePath
setDrive x drv = joinDrive drv (dropDrive x)

-- | Get the drive from a filepath.
--
-- > getDrive x == fst (splitDrive x)
getDrive :: FilePath -> FilePath
getDrive = fst . splitDrive

-- | Delete the drive, if it exists.
--
-- > dropDrive x == snd (splitDrive x)
dropDrive :: FilePath -> FilePath
dropDrive = snd . splitDrive

-- | Does a path have a drive.
--
-- > not (hasDrive x) == null (getDrive x)
hasDrive :: FilePath -> Bool
hasDrive = not . null . getDrive




---------------------------------------------------------------------
-- Operations on a filepath, as a list of directories

-- | Split a filename into directory and file.
--
-- > uncurry (++) (splitFileName x) == x
-- > splitFileName "file/bob.txt" == ("file/", "bob.txt")
-- > splitFileName "file/" == ("file/", "")
-- > splitFileName "bob" == ("", "bob")
-- > Posix:   splitFileName "/" == ("/","")
-- > Windows: splitFileName "c:" == ("c:","")
splitFileName :: FilePath -> (String, String)
splitFileName x = (c ++ reverse b, reverse a)
    where
        (a,b) = break isPathSeparator $ reverse d
        (c,d) = splitDrive x


-- | Join a directory and filename.
--
-- > uncurry joinFileName (splitFileName x) == x
joinFileName :: FilePath -> String -> FilePath
joinFileName x y = addFileName x y


-- | Add a filename onto the end of a path.
--
-- > addFileName (getDirectory x) (getFileName x) `equalFilePath` x
addFileName :: FilePath -> String -> FilePath
addFileName x y = if null x then y
                  else if null y then x
                  else if isPathSeparator (last x) then x ++ y
                  else x ++ [pathSeparator] ++ y

-- | Set the filename.
--
-- > setFileName x (getFileName x) == x
setFileName :: FilePath -> String -> FilePath
setFileName x y = joinFileName (fst $ splitFileName x) y

-- | Drop the filename.
--
-- > dropFileName x == fst (splitFileName x)
dropFileName :: FilePath -> FilePath
dropFileName x = fst (splitFileName x)


-- | Get the file name.
--
-- > getFileName "test/" == ""
-- > getFileName x == snd (splitFileName x)
-- > getFileName (setFileName x "fred") == "fred"
-- > getFileName (addFileName x "fred") == "fred"
getFileName :: FilePath -> FilePath
getFileName x = snd $ splitFileName x

-- | Get the base name, without an extension or path.
--
-- > getBaseName "file/test.txt" == "test"
-- > getBaseName "dave.ext" == "dave"
getBaseName :: FilePath -> String
getBaseName = dropExtension . getFileName

-- | Set the base name.
--
-- > setBaseName "file/test.txt" "bob" == "file/bob.txt"
-- > setBaseName "fred" "bill" == "bill"
setBaseName :: FilePath -> String -> FilePath
setBaseName pth nam = joinFileName a (joinExtension nam d)
    where
        (a,b) = splitFileName pth
        (c,d) = splitExtension b

-- | Is an item either a directory or the last character a path separator?
--   This does not query the file system.
--
-- > isDirectory "test" == False
-- > isDirectory "test/" == True
isDirectory :: FilePath -> Bool
isDirectory "" = False
isDirectory x = isPathSeparator (last x)

-- | Get the directory name, move up one level.
--
-- > Posix:    getDirectory "/foo/bar/baz" == "/foo/bar"
-- > Posix:    getDirectory "/foo/bar/baz/" == "/foo/bar/baz"
getDirectory :: FilePath -> FilePath
getDirectory x = a ++ if null res then file else res
    where
        (a,b) = splitDrive x
        res = reverse $ dropWhile isPathSeparator $ reverse file
        file = dropFileName b

-- | Set the directory, keeping the filename the same.
--
-- > setDirectory x (getDirectory x) `equalFilePath` x
setDirectory :: FilePath -> String -> FilePath
setDirectory x dir = joinFileName dir (getFileName x)


-- | Combine two paths, if the right path 'isAbsolute', then it returns the second.
--
-- > Posix:   combine "/" "test" == "/test"
-- > Posix:   combine "home" "bob" == "home/bob"
-- > Windows: combine "home" "bob" == "home\\bob"
combine :: FilePath -> FilePath -> FilePath
combine a b | isAbsolute b = b
            | otherwise = combineAlways a b

-- | Combine two paths, assuming rhs is NOT absolute.
combineAlways :: FilePath -> FilePath -> FilePath
combineAlways a b | null a = b
                  | null b = a
                  | isPathSeparator (last a) = a ++ b
                  | otherwise = a ++ [pathSeparator] ++ b

-- | A nice alias for 'combine'. E.g. @\"home\" '</>' \"bob\" -> \"home\/bob\"@
--   on Unix or @\"home\\bob\"@ on Windows.
(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine


-- | Split a path by the directory seperator. 
--
-- > concat (splitPath x) == x
-- > splitPath "test//item/" == ["test//","item/"]
-- > splitPath "test/item/file" == ["test/","item/","file"]
-- > splitPath "" == []
-- > Windows: splitPath "c:\\test\\path" == ["c:\\","test\\","path"]
-- > Posix:   splitPath "/file/test" == ["/","file/","test"]
splitPath :: FilePath -> [FilePath]
splitPath x = [a | a /= ""] ++ f b
    where
        (a,b) = splitDrive x
        
        f "" = []
        f x = (a++c) : f d
            where
                (a,b) = break isPathSeparator x
                (c,d) = break (not . isPathSeparator) b

-- | Just as 'splitPath', but don't add the trailing slashes to each element.
--
-- > splitDirectories "test/file" == ["test","file"]
-- > splitDirectories "/test/file" == ["/","test","file"]
-- > joinPath (splitDirectories x) `equalFilePath` x
-- > splitDirectories "" == []
splitDirectories :: FilePath -> [FilePath]
splitDirectories x =
        if hasDrive x then head xs : f (tail xs)
        else f xs
    where
        xs = splitPath x
        
        f xs = map g xs
        g x = if null res then x else res
            where res = takeWhile (not . isPathSeparator) x


-- | Join path elements back together.
--
-- > joinPath (splitPath x) == x
joinPath :: [FilePath] -> FilePath
joinPath x = foldr combineAlways "" x





---------------------------------------------------------------------
-- File name manipulators

-- | Find the canoncial path, if the file exists then the case will be correct
--   on Windows, if the file is a directory it will have a trailing slash
--   appended on all operating systems.
canonicalPath :: FilePath -> IO FilePath
canonicalPath x = do
        x2 <- fixElements $ normalise x
        x3 <- fixSlash x2
        return x3
    where
        fixElements x | isPosix = return x
        fixElements x = do
                fixed <- mapM (uncurry fixElement) tests
                return $ joinPath $ [head dirs | isAbsolute x] ++ fixed
            where
                tests = map (\x -> (joinPath $ init x, last x)) $ drop 2 $ inits dirs
                dirs = ["."|isRelative x] ++ splitDirectories x
            
        
        -- take a context, and an element, and return the new element
        fixElement :: FilePath -> FilePath -> IO FilePath
        fixElement context element = do
            let item = context </> element
            b1 <- doesDirectoryExist item
            b2 <- doesFileExist item
            if b1 || b2 then do
                items <- getDirectoryContents context
                let uelement = map toUpper element
                    i = [x | x <- items, map toUpper x == uelement]
                if null i then return element else return $ head i
             else
                return element
        
        fixSlash :: FilePath -> IO FilePath
        fixSlash file = do
            b <- doesDirectoryExist file
            if b && not (isDirectory file) then
                return $ file ++ [pathSeparator]
             else
                return file


-- | Equality of two 'FilePaths'. If you call 'fullPath' first this has a much
--   better chance of working. Note that this doesn't follow symlinks or
--   DOSNAM~1s. 
equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath a b = f a == f b
    where
        f x | isPosix   = dropTrailSlash $ normalise x
            | isWindows = dropTrailSlash $ map toLower $ normalise x
        
        dropTrailSlash "" = ""
        dropTrailSlash x | isPathSeparator (last x) = init x
                         | otherwise = x

-- | Expand out a filename to its full name, with the a directory factored in.
--
-- > Posix:   fullPathWith "/file/test/" "/bob/dave" == "/bob/dave"
-- > Posix:   fullPathWith "/file/test/" "bob" == "/file/test/bob"
-- > Posix:   fullPathWith "/file/test/" "../bob" == "/file/bob"
fullPathWith :: FilePath -> FilePath -> FilePath
fullPathWith cur x = normalise $ combine cur x

-- | 'fullPathWith' the current directory.
fullPath :: FilePath -> IO FilePath
fullPath x = do cur <- getCurrentDirectory
                return $ fullPathWith cur x

-- | Contract a filename, based on a relative path.
--
-- > Posix:   shortPathWith "/home/" "/home/bob/foo/bar" == "bob/foo/bar"
-- > Posix:   shortPathWith "/fred" "bob" == "bob"
-- > Posix:   shortPathWith "/file/test" "/file/test/fred" == "fred"
-- > Posix:   shortPathWith "/file/test" "/file/test/fred/" == "fred/"
-- > Posix:   shortPathWith "/fred/dave" "/fred/bill" == "../bill"
shortPathWith :: FilePath -> FilePath -> FilePath
shortPathWith cur x | isRelative x || isRelative cur || getDrive x /= getDrive cur = normalise x
shortPathWith cur x = joinPath $
                      replicate (length curdir - common) ".." ++
                      drop common orgpth
    where
        common = length $ takeWhile id $ zipWith (==) orgdir curdir
        orgpth = splitPath pth
        orgdir = splitDirectories pth
        curdir = splitDirectories $ dropDrive $ normalise $ cur
        (drv,pth) = splitDrive $ normalise x

-- | 'shortPathWith' the current directory.
shortPath :: FilePath -> IO FilePath
shortPath x = do cur <- getCurrentDirectory
                 return $ shortPathWith cur x


-- | Normalise a file
--
-- * \/\/ outside of the drive can be made blank
--
-- * \/ -> 'pathSeparator'
--
-- * .\/ -> \"\"
--
-- * item\/..\/ -> \"\"
--
-- > Posix:   normalise "/file/\\test////" == "/file/\\test/"
-- > Posix:   normalise "/file/./test" == "/file/test"
-- > Posix:   normalise "/test/file/../bob/fred/" == "/test/bob/fred/"
-- > Posix:   normalise "../bob/fred/" == "../bob/fred/"
-- > Posix:   normalise "./bob/fred/" == "bob/fred/"
-- > Windows: normalise "c:\\file/bob\\" == "C:\\file\\bob\\"
-- > Windows: normalise "\\\\server\\test" == "\\\\server\\test"
-- > Windows: normalise "c:/file" == "C:\\file"
normalise :: FilePath -> FilePath
normalise "" = ""
normalise x = joinDrive (normaliseDrive drv) (f pth) ++ [pathSeparator | isPathSeparator $ last x]
    where
        (drv,pth) = splitDrive x
    
        f = joinPath . dropDots [] . splitDirectories . propSep
        
        g x = if isPathSeparator x then pathSeparator else x
    
        propSep (a:b:xs) | isPathSeparator a && isPathSeparator b = propSep (a:xs)
        propSep (a:xs) | isPathSeparator a = pathSeparator : propSep xs
        propSep (x:xs) = x : propSep xs
        propSep [] = []
        
        dropDots acc (".":xs) = dropDots acc xs
        dropDots (a:cc) ("..":xs) = dropDots cc xs
        dropDots [] ("..":xs) = ".." : dropDots [] xs
        dropDots acc (x:xs) = dropDots (x:acc) xs
        dropDots acc [] = reverse acc

normaliseDrive :: FilePath -> FilePath
normaliseDrive x | isPosix = x
normaliseDrive x = if isJust $ readDriveLetter x2 then
                       map toUpper x2
                   else
                       x
    where
        x2 = map repSlash x
        
        repSlash x = if isPathSeparator x then pathSeparator else x

-- information for validity functions on Windows


-- see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/fileio/fs/naming_a_file.asp
badCharacters = ":*?><|"
badElements = ["CON", "PRN", "AUX", "NUL", "COM1", "COM2", "COM3", "COM4", "COM5", "COM6", "COM7", "COM8", "COM9", "LPT1", "LPT2", "LPT3", "LPT4", "LPT5", "LPT6", "LPT7", "LPT8", "LPT9", "CLOCK$"]


-- | Is a FilePath valid, i.e. could you create a file like it?
--
-- > Posix:   isValid "/random_ path:*" == True
-- > Posix:   isValid x == True
-- > Windows: isValid "c:\\test" == True
-- > Windows: isValid "c:\\test:of_test" == False
-- > Windows: isValid "test*" == False
-- > Windows: isValid "c:\\test\\nul" == False
-- > Windows: isValid "c:\\test\\prn.txt" == False
-- > Windows: isValid "c:\\nul\\file" == False
isValid :: FilePath -> Bool
isValid x | isPosix = True
isValid x = not (any (`elem` badCharacters) x2) && not (any f $ splitDirectories x2)
    where
        x2 = dropDrive x
        f x = map toUpper (dropExtensions x) `elem` badElements
    

-- | Take a FilePath and make it valid; does not change already valid FilePaths.
--
-- > isValid (makeValid x)
-- > if isValid x then makeValid x == x else True
-- > Windows: makeValid "c:\\test:of_test" == "c:\\test_of_test"
-- > Windows: makeValid "test*" == "test_"
-- > Windows: makeValid "c:\\test\\nul" == "c:\\test\\nul_"
-- > Windows: makeValid "c:\\test\\prn.txt" == "c:\\test\\prn_.txt"
-- > Windows: makeValid "c:\\test/prn.txt" == "c:\\test/prn_.txt"
-- > Windows: makeValid "c:\\nul\\file" == "c:\\nul_\\file"
makeValid :: FilePath -> FilePath
makeValid x | isPosix = x
makeValid x = joinDrive drv $ validElements $ validChars pth
    where
        (drv,pth) = splitDrive x
        
        validChars x = map f x
        f x | x `elem` badCharacters = '_'
            | otherwise = x

        validElements x = joinPath $ map g $ splitPath x
        g x = h (reverse b) ++ reverse a
            where (a,b) = span isPathSeparator $ reverse x
        h x = if map toUpper a `elem` badElements then joinExtension (a ++ "_") b else x
            where (a,b) = splitExtensions x
        

-- | Is a path relative, or is it fixed to the root?
--
-- > Windows: isRelative "path\\test" == True
-- > Windows: isRelative "c:\\test" == False
-- > Posix:   isRelative "test/path" == True
-- > Posix:   isRelative "/test" == False
isRelative :: FilePath -> Bool
isRelative x = null $ getDrive x


-- | @not . 'isRelative'@
--
-- > isAbsolute x == not (isRelative x)
isAbsolute :: FilePath -> Bool
isAbsolute = not . isRelative



-- Search Methods

-- | Get a list of all the directories within this directory.
getDirectoryList :: FilePath -> IO [String]
getDirectoryList path = do x <- getDirectoryContents path
                           let xfull = filter (not . isFakeDirectory) x
                           filterM (\a -> doesDirectoryExist $ combine path a) xfull

-- | Makes a directory and all its parents (mkdir -p). For example 
--   ensureDirectory \".\/One\/Two\/Three\" would create the directory \"Two\" 
--   and \"Three\" if \".\" and \"One\" already existed.
ensureDirectory :: FilePath -> IO ()
ensureDirectory path = when (not $ null pths) $ f (joinDrive drv (head pths)) (tail pths)
    where
        pths = splitPath pth
        (drv,pth) = splitDrive path
    
        f pth todo = do
            exist <- doesDirectoryExist pth
            when (not exist) $ createDirectory pth
            case todo of
                (t:odo) -> f (pth </> t) odo
                [] -> return ()


-- | Is a directory a real directory, or an alias to a parent . or ..?
isFakeDirectory :: FilePath -> Bool
isFakeDirectory x = x == "." || x == ".."

-- Temporary File Names

-- | Get a temporary file name.
getTemporaryFile :: String -> IO FilePath
getTemporaryFile ext = getTemporaryFileSeed 1 ext

-- | Get a temporary file name, using a specified number as a seed.
getTemporaryFileSeed :: Int -> String -> IO FilePath
getTemporaryFileSeed n ext = do
    prog <- getProgName
    tmpdir <- getTemporaryDirectory
    return $ makeValid $ tmpdir </> (prog ++ show n) <.> ext
    
-- | Get a temporary file name which does not exist.
--   Beware of race conditions, the file may be created after this function
--   returns. Nothing may be returned if a new item is not found in 100 tries.
getTemporaryFileNew :: String -> IO (Maybe FilePath)
getTemporaryFileNew ext = f [1..100]
    where
        f [] = return Nothing
        f (x:xs) = do fil <- getTemporaryFileSeed x ext
                      b <- doesFileExist fil
                      if b then f xs else return $ Just fil
