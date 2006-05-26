
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
    FilePath,
    pathSeparator, isPathSeparator,
    fileSeparator, isFileSeparator,
    extSeparator, isExtSeparator,
    
    getExtension, setExtension, addExtension, dropExtension, hasExtension, (<.>),
    )
    where

import Data.Maybe(isJust)
import Data.Char(toLower)
import Data.List(isPrefixOf)

import System.Info(os, compilerName)

import System.Environment(getEnv)
import System.Directory(getCurrentDirectory, getDirectoryContents, doesDirectoryExist, createDirectory)


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
isUnix :: Bool
isUnix = not isWindows && forceEffectView /= ForceWindows

-- | Is the operating system Windows like
isWindows :: Bool
isWindows = osName == "windows" && forceEffectView /= ForcePosix


-- FIXME - windows should have \\ first, but this breaks various assumptions in
-- the compiler, its a bit unix specific

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
splitPath :: String -> [FilePath]
splitPath var = do f var
    where
        f xs = if null pre && null post then []
                 else if null pre then f (tail post)
                 else if null post then [pre]
                 else pre : f (tail post)
            where (pre, post) = break isFileSeparator xs

-- | Get a list of filepaths in the $PATH
getPath :: IO [FilePath]
getPath = do variable <- getEnv "PATH"
             return $ splitPath variable


-- * Name Expansion and Contraction

-- | If you call getFullName first this has a much better chance of working!
pathEqual :: FilePath -> FilePath -> Bool
pathEqual a b = f a == f b
    where
        f x | isUnix    = normaliseSlash x
            | isWindows = map toLower $ normaliseSlash x


-- | Expand out a filename to its full name, with the current directory factored in
getFullName :: FilePath -> IO FilePath
getFullName x = do cur <- getCurrentDirectory
                   return $ combine cur x

-- | Contract a filename, based on its current directory
getShortName :: FilePath -> IO FilePath
getShortName x =
    do
        cur <- getCurrentDirectory
        let xelem = getPathElements (normalisePath x)
            (rescur,resfile) = f (getPathElements cur) xelem
            lrescur = length rescur
            
        return $
            if lrescur == 0 then
                joinPathElements $ dropLeadingSlash resfile
            else if lrescur < 3 then
                joinPathElements (replicate lrescur ".." ++ resfile)
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

-- | Get the extension of a file, returns @\"\"@ for no extension, @.ext@ otherwise
getExtension :: FilePath -> String
getExtension x = if hasExtension x2
                 then extSeparator : (reverse $ takeWhile (not . isExtSeparator) $ reverse x2)
                 else ""
    where x2 = getFileName x

-- | Set the extension of a file, overwriting one if already present
setExtension :: FilePath -> String -> FilePath
setExtension file "" = dropExtension file
setExtension file (x:xs) | isExtSeparator x = setExtension file xs
setExtension file xs = addExtension (dropExtension file) xs

-- | Alias, for people who like that sort of thing
--   Probably needs a fixity and precedence...
(<.>) :: FilePath -> String -> FilePath
(<.>) = setExtension

-- | Remove the extension, and any . following it
dropExtension :: FilePath -> FilePath
dropExtension x = reverse $ drop lext $ reverse x
    where lext = length $ getExtension x

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
getDrive x | isUnix = ""
getDrive ('\\':'\\':xs) = "\\\\" ++ takeWhile (not . isPathSeparator) xs
getDrive (x:':':_) = [x,':']
getDrive _ = ""


-- | Remove the drive from a file path, if it has one
dropDrive :: FilePath -> FilePath
dropDrive x = drop (length $ getDrive x) x


-- | Get the directory name, move up one level
getDirectoryName :: FilePath -> FilePath
getDirectoryName x = case getPathElements x of
                        [] -> ""
                        xs -> concat (init xs)

-- | Get the file name
getFileName :: FilePath -> FilePath
getFileName x = case getPathElements (dropDrive x) of
                    [] -> ""
                    xs -> dropWhile isPathSeparator $ last xs


-- | return each path, concat res = input
--   unless there is a trailing slash, in which case its deleted
--   and multiple slashes after each other are removed
--
--   slashes are attached to the front of each element
--   i.e. \/file\/path -> [\"\/file\", \"\/path\"]
getPathElements :: FilePath -> [FilePath]
getPathElements x = if null drive
                    then f "" x2
                    else drive : f "" (dropDrive x2)
    where
        x2 = normaliseSlash x
        drive = getDrive x2
    
        f acc [] = outAcc acc []
        f acc (x:xs) | isPathSeparator x = outAcc acc $ f [x] xs
                     | otherwise = f (x:acc) xs
        
        outAcc [] rest = rest
        outAcc a  rest = reverse a : rest


-- | Join path elements back together
joinPathElements :: [FilePath] -> FilePath
joinPathElements [] = ""
joinPathElements (x:xs) = x ++ concatMap f xs
    where
        f xs@(x:_) | isPathSeparator x = xs
        f x = pathSeparator : x


-- | remove the slash at the end, if there is one
-- | remove multiple adjacent slashes
-- | ignore the drive name
normaliseSlash :: FilePath -> FilePath
normaliseSlash x = drive ++ f (dropDrive x)
    where
        drive = getDrive x
        
        f [x] | isPathSeparator x = []
        f (a:b:xs) | isPathSeparator a && isPathSeparator b = f (a:xs)
        f (x:xs) = x : f xs
        f [] = []

-- | remove .\/ and ..\/x\/
normalisePath :: FilePath -> FilePath
normalisePath = joinPathElements . f . getPathElements . normaliseSlash
    where
        f ([y,'.']:x) | isPathSeparator y = f x
        f ([y,'.','.']:x:xs) | isPathSeparator y = f xs
        f (x:xs) = x : f xs
        f [] = []


-- * Combine Two Existing Paths

-- | Is a path relative, or is it fixed to the root
isRelative :: FilePath -> Bool
isRelative x | isUnix = not $ "/" `isPrefixOf` x
isRelative x = null $ getDrive x


-- | not . 'isRelative'
isAbsolute :: FilePath -> Bool
isAbsolute = not . isRelative


-- | Combine two paths, if the right path 'isAbsolute', then it returns the second
combine :: FilePath -> FilePath -> FilePath
combine a b = if isRelative b
              then f (getPathElements a) (getPathElements b)
              else b
    where
        f [] ys = joinPathElements ys
        f xs (y:ys) | isJust move = case move of
                Just 0 -> f xs ys
                Just 1 -> f (init xs) ys
            where
                move = g y
        f xs ys = joinPathElements (xs ++ ys)
        
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
ensureDirectory path = f $ getPathElements path
    where
        f [] = return ()
        f xs =
            do let (initx,lastx) = (init xs, last xs)
                   path = joinPathElements xs
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


