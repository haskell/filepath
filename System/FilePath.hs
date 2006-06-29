
{- |
    Module      :  System.FilePath
    Copyright   :  (c) Neil Mitchell 2005-2006
    License     :  BSD3
 
    Maintainer  :  http://www.cs.york.ac.uk/~ndm/
    Stability   :  in-progress
    Portability :  portable

    A library for FilePath manipulations, designed to be cross platform.
    This library will select the correct type of FilePath's for the
    platform the code is running on at runtime.

    For more details see <http://www.cs.york.ac.uk/~ndm/projects/libraries.php>
-}

module System.FilePath
    (
    -- * The basic functions
    FilePath,
    pathSeparator, isPathSeparator,
    fileSeparator, isFileSeparator,
    extSeparator, isExtSeparator,
    
    -- * Path methods (environment $PATH)
    splitFiles, getPath,
    
    -- * Extension methods
    splitExtension, joinExtension,
    getExtension, setExtension, dropExtension, addExtension, hasExtension, (<.>),
    
    -- * Drive methods
    splitDrive, joinDrive,
    getDrive, setDrive, hasDrive, dropDrive,
    
    -- * Operations on a filepath, as a list of directories
    splitFileName, joinFileName,
    getFileName, setFileName, dropFileName, addFileName,
    getDirectory, setDirectory, isDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,
    
    -- * File name manipulators
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

import Data.Maybe(isJust, fromMaybe)
import Data.Char(toLower)
import Data.List(isPrefixOf)
import Control.Monad(when)

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

-- | A list of the possible path separators, Unix = @\/@, Windows = @\/\\@.
--   These go between path elements in a file
pathSeparator :: Char
pathSeparator = if isWindows then '\\' else '/'

isPathSeparator :: Char -> Bool
isPathSeparator x = x `elem` (if isWindows then "\\/" else "/")


-- | A list of possible file separators, between the $PATH variable
--   Windows = @;@, Unix = @:@
fileSeparator :: Char
fileSeparator = if isWindows then ';' else ':'

isFileSeparator :: Char -> Bool
isFileSeparator x = x == fileSeparator


-- | File extension character, '.' on all systems
extSeparator :: Char
extSeparator = '.'

isExtSeparator :: Char -> Bool
isExtSeparator x = x == extSeparator





---------------------------------------------------------------------
-- Path methods (environment $PATH)

-- | Take a string, split it on the "fileSeparators" character
splitFiles :: String -> [FilePath]
splitFiles var = do f var
    where
        f xs = if null pre && null post then []
               else if null pre then f (tail post)
               else if null post then [pre]
               else pre : f (tail post)
            where (pre, post) = break isFileSeparator xs

-- | Get a list of filepaths in the $PATH
getPath :: IO [FilePath]
getPath = do variable <- getEnv "PATH"
             return $ splitFiles variable


---------------------------------------------------------------------
-- Extension methods

splitExtension :: FilePath -> (String, String)
splitExtension x = case d of
                       "" -> (x,"")
                       (y:ys) -> (a ++ reverse ys, y : reverse c)
    where
        (a,b) = splitFileName x
        (c,d) = break isExtSeparator $ reverse b


joinExtension :: String -> String -> FilePath
joinExtension = addExtension

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise
getExtension :: FilePath -> String
getExtension x = snd $ splitExtension x

-- | Set the extension of a file, overwriting one if already present
setExtension :: FilePath -> String -> FilePath
setExtension x y = joinExtension a y
    where (a,b) = splitExtension x

-- | Alias, for people who like that sort of thing
--   Probably needs a fixity and precedence...
(<.>) :: FilePath -> String -> FilePath
(<.>) = addExtension

-- | Remove last extension, and any . following it
dropExtension :: FilePath -> FilePath
dropExtension x = fst $ splitExtension x

-- | Add an extension, even if there is already one there
addExtension :: FilePath -> String -> FilePath
addExtension file "" = file
addExtension file xs@(x:_) | isExtSeparator x = file ++ xs
                           | otherwise = file ++ [extSeparator] ++ xs

-- | Does the given filename have an extension
hasExtension :: FilePath -> Bool
hasExtension x = any isExtSeparator $ getFileName x





---------------------------------------------------------------------
-- Drive methods

-- only a-z and A-Z are letters, not isAlpha which is more unicodey
isLetter :: Char -> Bool
isLetter x | x >= 'a' && x <= 'z' = True
           | x >= 'A' && x <= 'Z' = True
           | otherwise = False

splitDrive :: FilePath -> (FilePath, FilePath)
splitDrive x | isPosix = case x of
                             '/':xs -> ("/",xs)
                             xs -> ("",xs)
splitDrive (x:':':[]) | isLetter x = ([x,':'],"")
         -- NOTE c:/ is not a valid path!
splitDrive (x:':':'\\':xs) | isLetter x = ([x,':','\\'],xs)
splitDrive ('\\':'\\':xs) = case b of
                               "" -> ("\\\\" ++ xs, "")
                               (y:ys) -> ("\\\\" ++ a ++ [y], ys)
    where (a,b) = break isPathSeparator xs
splitDrive x = ("",x)


joinDrive :: FilePath -> FilePath -> FilePath
joinDrive a b | isPosix = a ++ b
              | null a = b
              | isPathSeparator (last a) = a ++ b
              | otherwise = a ++ [pathSeparator] ++ b

setDrive :: FilePath -> String -> FilePath
setDrive x drv = joinDrive drv (dropDrive x)

getDrive :: FilePath -> FilePath
getDrive = fst . splitDrive

dropDrive :: FilePath -> FilePath
dropDrive = snd . splitDrive

hasDrive :: FilePath -> Bool
hasDrive = not . null . getDrive




---------------------------------------------------------------------
-- Operations on a filepath, as a list of directories

splitFileName :: FilePath -> (String, String)
splitFileName x = (c ++ reverse b, reverse a)
    where
        (a,b) = break isPathSeparator $ reverse d
        (c,d) = splitDrive x


joinFileName :: FilePath -> String -> FilePath
joinFileName x y = addFileName x y


addFileName :: FilePath -> String -> FilePath
addFileName x y = if null x then y
                  else if isPathSeparator (last x) then x ++ y
                  else x ++ [pathSeparator] ++ y

setFileName :: FilePath -> String -> FilePath
setFileName x y = joinFileName (fst $ splitFileName x) y

dropFileName :: FilePath -> FilePath
dropFileName x = reverse $ dropWhile (not . isPathSeparator) $ reverse x


-- | Get the file name
getFileName :: FilePath -> FilePath
getFileName x = snd $ splitFileName x


isDirectory :: FilePath -> Bool
isDirectory "" = False
isDirectory x = isPathSeparator (last x)

-- | Get the directory name, move up one level
getDirectory :: FilePath -> FilePath
getDirectory x = if null res then file else res
    where
        res = reverse $ dropWhile isPathSeparator $ reverse file
        file = dropFileName x

setDirectory :: FilePath -> String -> FilePath
setDirectory x dir = joinFileName dir (getFileName x)


-- | Combine two paths, if the right path 'isAbsolute', then it returns the second
combine :: FilePath -> FilePath -> FilePath
combine a b | isAbsolute b = b
            | otherwise = combineAlways a b

-- | Combine two paths, assuming rhs is NOT absolute
combineAlways :: FilePath -> FilePath -> FilePath
combineAlways a b | null a = b
                  | null b = a
                  | isPathSeparator (last a) = a ++ b
                  | otherwise = a ++ [pathSeparator] ++ b

-- | A nice alias for 'combine'
--   Probably needs a fixity and precedence...
(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine


-- | return each path, concat res = input
--   slashes go at the end of each element
splitPath :: FilePath -> [FilePath]
splitPath x = [a | a /= ""] ++ f b
    where
        (a,b) = splitDrive x
        
        f "" = []
        f x = (a++c) : f d
            where
                (a,b) = break isPathSeparator x
                (c,d) = break (not . isPathSeparator) b

-- | return all the elements of the path
--   without trailing slashes
splitDirectories :: FilePath -> [FilePath]
splitDirectories x =
        if hasDrive x then head xs : f (tail xs)
        else f xs
    where
        xs = splitPath x
        
        f xs = map g xs
        g x = takeWhile (not . isPathSeparator) x


-- | Join path elements back together
joinPath :: [FilePath] -> FilePath
joinPath x = foldr combineAlways "" x





---------------------------------------------------------------------
-- File name manipulators

-- | If you call 'fullFilePath' first this has a much better chance of working!
equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath a b = f a == f b
    where
        f x | isPosix   = dropTrailSlash $ normalise x
            | isWindows = dropTrailSlash $ map toLower $ normalise x
        
        dropTrailSlash "" = ""
        dropTrailSlash x | isPathSeparator (last x) = init x
                         | otherwise = x

-- | Expand out a filename to its full name, with the current directory factored in
fullPathWith :: FilePath -> FilePath -> FilePath
fullPathWith cur x = normalise $ combine cur x

fullPath :: FilePath -> IO FilePath
fullPath x = do cur <- getCurrentDirectory
                return $ fullPathWith cur x

-- | Contract a filename, based on its current directory
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


shortPath :: FilePath -> IO FilePath
shortPath x = do cur <- getCurrentDirectory
                 return $ shortPathWith cur x


-- | normalise a file
--   \/\/ outside of the drive can be made blank
--   \/ -> pathSeparator
--   .\/ -> \"\"
--   item\/..\/ -> \"\"
normalise :: FilePath -> FilePath
normalise "" = ""
normalise x = joinDrive drv (f pth) ++ [pathSeparator | isPathSeparator $ last x]
    where
        (drv,pth) = splitDrive x
    
        f = joinPath . dropDots [] . splitDirectories . propSep
    
        propSep (a:b:xs) | isPathSeparator a && isPathSeparator b = propSep (a:xs)
        propSep (a:xs) | isPathSeparator a = pathSeparator : propSep xs
        propSep (x:xs) = x : propSep xs
        propSep [] = []
        
        dropDots acc (".":xs) = dropDots acc xs
        dropDots (a:cc) ("..":xs) = dropDots cc xs
        dropDots [] ("..":xs) = ".." : dropDots [] xs
        dropDots acc (x:xs) = dropDots (x:acc) xs
        dropDots acc [] = reverse acc
        

badCharacters = ":*?><|"

isValid :: FilePath -> Bool
isValid x | isPosix = False
isValid x = not $ any (`elem` badCharacters) $ dropDrive x
    

makeValid :: FilePath -> FilePath
makeValid x | isPosix = x
makeValid x = joinDrive drv (map f pth)
    where
        (drv,pth) = splitDrive x
        
        f x | x `elem` badCharacters = '_'
            | otherwise = x


-- | Is a path relative, or is it fixed to the root
isRelative :: FilePath -> Bool
isRelative x = null $ getDrive x


-- | not . 'isRelative'
isAbsolute :: FilePath -> Bool
isAbsolute = not . isRelative



-- Search Methods

-- | Get a list of all the directories within this directory
getDirectoryList :: FilePath -> IO [String]
getDirectoryList path = do x <- getDirectoryContents path
                           let xfull = filter (not . isFakeDirectory) x
                           filterM (\a -> doesDirectoryExist $ combine path a) xfull

-- | Makes a directory and all it's parents
-- |   for example ensureDirectory \".\/One\/Two\/Three\"
-- | would create the directory \"Two\" and \"Three\" if \".\" and \"One\" already existed.
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


-- | Is a directory a real directory, or an alias to a parent . or ..
isFakeDirectory x = x == "." || x == ".."


filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f [] = return []
filterM f (x:xs) = do res <- f x
                      rest <- filterM f xs
                      return $ if res then x : rest else rest

-- Temporary File Names


getTemporaryFile :: String -> IO FilePath
getTemporaryFile ext = getTemporaryFileSeed 1 ext


getTemporaryFileSeed :: Int -> String -> IO FilePath
getTemporaryFileSeed n ext = do
    prog <- getProgName
    tmpdir <- getTemporaryDirectory
    return $ makeValid $ tmpdir </> (prog ++ show n) <.> ext
    

getTemporaryFileNew :: String -> IO (Maybe FilePath)
getTemporaryFileNew ext = f [1..100]
    where
        f [] = return Nothing
        f (x:xs) = do fil <- getTemporaryFileSeed x ext
                      b <- doesFileExist fil
                      if b then f xs else return $ Just fil
