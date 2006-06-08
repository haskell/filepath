
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
    
    -- * Extension methods
    splitExtension, joinExtension,
    getExtension, setExtension, addExtension, dropExtension, hasExtension, (<.>),
    
    -- * Drive methods
    getDrive,
    
    -- * Operations on a filepath, as a list of directories
    splitFileName, joinFileName,
    dropFileName, getFileName, setFileName, addFileName,
    splitPath, joinPath,
    combine, (</>),
    
    -- * File name manipulators
    equalFilePath, fullFilePath, shortFilePath,
    isRelative, isAbsolute, isValid, makeValid,
    normaliseSlash, normalisePath, normalise,
    
    -- * Path methods (environment $PATH)
    splitFiles, getPath,
    
    -- * Directory operations
    getDirectoryList, ensureDirectory,
    
    -- * Temporary file operations
    getNewTemporaryDirectory, -- is this name too long? should it be getTmpDir?
    getNewTemporaryFilename
    )
    where

import Data.Maybe(isJust, fromMaybe)
import Data.Char(toLower)
import Data.List(isPrefixOf)

import System.Info(os, compilerName)

import System.Environment(getEnv)
import System.Directory(getCurrentDirectory, getDirectoryContents, doesFileExist, doesDirectoryExist, createDirectory)


infixr 7  <.>
infixr 5  </>


-- * Platform Abstraction Methods (private)

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


-- * $PATH methods

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


-- * Name Expansion and Contraction

-- | If you call 'getFullName' first this has a much better chance of working!
equalFilePath :: FilePath -> FilePath -> Bool
equalFilePath a b = f a == f b
    where
        f x | isPosix   = normaliseSlash x
            | isWindows = map toLower $ normaliseSlash x


-- | Expand out a filename to its full name, with the current directory factored in
fullFilePath :: FilePath -> IO FilePath
fullFilePath x = do cur <- getCurrentDirectory
                    return $ combine cur x

-- | Contract a filename, based on its current directory
shortFilePath :: FilePath -> IO FilePath
shortFilePath x =
    do
        cur <- getCurrentDirectory
        let xelem = splitPath (normalisePath x)
            (rescur,resfile) = f (splitPath cur) xelem
            lrescur = length rescur
            
        return $
            if lrescur == 0 then
                joinPath $ dropLeadingSlash resfile
            else if lrescur < 3 then
                joinPath (replicate lrescur ".." ++ resfile)
            else
                x
    where
        f (a:as) (b:bs) | noLeadingSlash a == noLeadingSlash b = f as bs
        f as bs = (as,bs)
        
        noLeadingSlash (x:xs) | isPathSeparator x = xs
        noLeadingSlash xs = xs
        
        dropLeadingSlash (x:xs) = noLeadingSlash x : xs
        dropLeadingSlash [] = []


-- * Extension Methods

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


-- * Split Up Path Elements

-- | Get the drive of the file path, on Unix this is a noop (returns @\"\"@)
--   On windows it handles share names (@\\\\swale@) and drive names (@C:@)
--   Do not return a trailing \\ character
getDrive :: FilePath -> FilePath
getDrive x | isPosix = ""
getDrive ('\\':'\\':xs) = "\\\\" ++ takeWhile (not . isPathSeparator) xs
getDrive (x:':':_) = [x,':']
getDrive _ = ""



-- | Get the directory name, move up one level
getDirectoryName :: FilePath -> FilePath
getDirectoryName x = case splitPath x of
                        [] -> ""
                        xs -> concat (init xs)


splitFileName :: FilePath -> (String, String)
splitFileName x = (reverse b, reverse a)
    where (a,b) = break isPathSeparator $ reverse x


joinFileName :: FilePath -> String -> FilePath
joinFileName x y = x ++ y


addFileName :: FilePath -> String -> FilePath
addFileName x y = x ++ y

setFileName :: FilePath -> String -> FilePath
setFileName x y = x ++ y

dropFileName :: FilePath -> FilePath
dropFileName x = x

-- | Get the file name
getFileName :: FilePath -> FilePath
getFileName x = snd $ splitFileName x


-- | return each path, concat res = input
--   unless there is a trailing slash, in which case its deleted
--   and multiple slashes after each other are removed
--
--   slashes are attached to the front of each element
--   i.e. \/file\/path -> [\"\/file\", \"\/path\"]
splitPath :: FilePath -> [FilePath]
splitPath x = f "" x2
    where
        x2 = normaliseSlash x
        drive = getDrive x2
    
        f acc [] = outAcc acc []
        f acc (x:xs) | isPathSeparator x = outAcc acc $ f [x] xs
                     | otherwise = f (x:acc) xs
        
        outAcc [] rest = rest
        outAcc a  rest = reverse a : rest


-- | Join path elements back together
joinPath :: [FilePath] -> FilePath
joinPath [] = ""
joinPath (x:xs) = x ++ concatMap f xs
    where
        f xs@(x:_) | isPathSeparator x = xs
        f x = pathSeparator : x


-- | remove the slash at the end, if there is one
-- | remove multiple adjacent slashes
-- | ignore the drive name
normaliseSlash :: FilePath -> FilePath
normaliseSlash x = f x
    where
        f [x] | isPathSeparator x = []
        f (a:b:xs) | isPathSeparator a && isPathSeparator b = f (a:xs)
        f (x:xs) = x : f xs
        f [] = []

-- | remove .\/ and ..\/x\/
normalisePath :: FilePath -> FilePath
normalisePath = joinPath . f . splitPath . normaliseSlash
    where
        f ([y,'.']:x) | isPathSeparator y = f x
        f ([y,'.','.']:x:xs) | isPathSeparator y = f xs
        f (x:xs) = x : f xs
        f [] = []

normalise :: FilePath -> FilePath
normalise = map f . normalisePath
    where
        f x | isPathSeparator x = pathSeparator
            | otherwise = x


isValid :: FilePath -> Bool
isValid _ = False

makeValid :: FilePath -> FilePath
makeValid x = x


-- * Combine Two Existing Paths

-- | Is a path relative, or is it fixed to the root
isRelative :: FilePath -> Bool
isRelative x | isPosix = not $ "/" `isPrefixOf` x
isRelative x = null $ getDrive x


-- | not . 'isRelative'
isAbsolute :: FilePath -> Bool
isAbsolute = not . isRelative


-- | Combine two paths, if the right path 'isAbsolute', then it returns the second
combine :: FilePath -> FilePath -> FilePath
combine a b = if isRelative b
              then f (splitPath a) (splitPath b)
              else b
    where
        f [] ys = joinPath ys
        f xs (y:ys) | isJust move = case move of
                Just 0 -> f xs ys
                Just 1 -> f (init xs) ys
            where
                move = g y
        f xs ys = joinPath (xs ++ ys)
        
        g (x:xs) | isPathSeparator x = g xs
        g "." = Just 0
        g ".." = Just 1
        g _ = Nothing

-- | A nice alias for 'combine'
--   Probably needs a fixity and precedence...
(</>) :: FilePath -> FilePath -> FilePath
(</>) = combine

-- * Search Methods

-- | Get a list of all the directories within this directory
getDirectoryList :: FilePath -> IO [String]
getDirectoryList path = do x <- getDirectoryContents path
                           let xfull = filter (not . isFakeDirectory) x
                           filterM (\a -> doesDirectoryExist $ combine path a) xfull

-- | Makes a directory and all it's parents
-- |   for example ensureDirectory \".\/One\/Two\/Three\"
-- | would create the directory \"Two\" and \"Three\" if \".\" and \"One\" already existed.
ensureDirectory :: FilePath -> IO ()
ensureDirectory path = f $ splitPath path
    where
        f [] = return ()
        f xs =
            do let (initx,lastx) = (init xs, last xs)
                   path = joinPath xs
               exists <- doesDirectoryExist path
               if exists then
                   return ()
                else
                   do f initx
                      createDirectory path

-- | Is a directory a real directory, or an alias to a parent . or ..
isFakeDirectory x = x == "." || x == ".."


filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f [] = return []
filterM f (x:xs) = do res <- f x
                      rest <- filterM f xs
                      return $ if res then x : rest else rest

-- * get temporary file and directory names (beta .. I think it con be done much better Marc)

-- | simply returns [prefix0suffix,prefix1suffix, ...]
getSystemTempDir :: IO String -- IO because System dedection might be IO, too
getSystemTempDir = do
  let win = isWindows
  if win then return "c:\\temp" -- should we take the users temp dir here?
	 else return "/tmp"

simpleTempNameProvider :: String -> String -> [String]
simpleTempNameProvider prefix suffix = [ prefix ++ (show i) ++ suffix | i <- [0..]]


-- | helper function returns new dir or file dependend on doesExist
getNewTemporaryFilePath_ :: (FilePath -> IO Bool) -> (Maybe FilePath)  ->
	[FilePath] -> IO FilePath
getNewTemporaryFilePath_ doesExist tmpDir list = do
  tmpDir_  <- maybe getSystemTempDir return tmpDir
  takeNextNameWhileExists tmpDir_  list
     where takeNextNameWhileExists tmpDir_ (name:list) =
	     let tempname = tmpDir_ </> name  in do
		 exists <-  doesExist tempname
		 if exists then takeNextNameWhileExists tmpDir_ list -- try next
			   else return tempname

-- | returns a temporary filepath named tmpdir0, tmpdir1 if it exists etc
-- | location in case Nothing is getSystemTempDir
-- | result can be used for getNewTemporaryFilename if your application needs more 
-- | than a few temporary files
getNewTemporaryDirectory :: (Maybe FilePath) -> (IO FilePath)
getNewTemporaryDirectory tmpDir = getNewTemporaryFilePath_ 
      doesFileExist  tmpDir (simpleTempNameProvider "tmpdir" "" )
    -- should we add the application name here woild be user friendly

-- | returns a temporary filename named tmp0.tmp, tmp1.tmp if it exists etc

-- | location in case Nothing is getSystemTempDir
getNewTemporaryFilename :: (Maybe FilePath) -> IO FilePath
getNewTemporaryFilename tmpDir = getNewTemporaryFilePath_ 
      doesDirectoryExist  tmpDir (simpleTempNameProvider "tmp" ".tmp" )
