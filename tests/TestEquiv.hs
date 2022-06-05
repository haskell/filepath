module Main where

import           Control.Monad
import           Data.Maybe
import           Data.Semigroup
import           Prelude                       as P
import           System.Environment
import           Test.QuickCheck         hiding ( (==>) )
import           TestUtil

import qualified Data.Char                     as C
import qualified Legacy.System.FilePath.Posix  as LP
import qualified Legacy.System.FilePath.Windows
                                               as LW
import qualified System.FilePath.Posix         as P
import qualified System.FilePath.Windows       as W


main :: IO ()
main = do
  args <- getArgs
  let count = case args of
        i : _ -> read i
        _     -> 10000
  let testNum = case args of
        _ : i : _
          | let num = read i, num < 0 -> drop (negate num) equivalentTests
          | let num = read i, num > 0 -> take num equivalentTests
          | otherwise                 -> []
        _ -> equivalentTests
  putStrLn $ "Testing with " ++ show count ++ " repetitions"
  let total = length testNum
  let showOutput x = show x { output = "" } ++ "\n" ++ output x
  bad <- fmap catMaybes $ forM (zip [1 ..] testNum) $ \(i, (msg, prop)) -> do
    putStrLn $ "Test " ++ show i ++ " of " ++ show total ++ ": " ++ msg
    res <- quickCheckWithResult stdArgs { chatty = False, maxSuccess = count }
                                prop
    case res of
      Success{} -> pure Nothing
      bad       -> do
        putStrLn $ showOutput bad
        putStrLn "TEST FAILURE!"
        pure $ Just (msg, bad)
  if null bad
    then putStrLn $ "Success, " ++ show total ++ " tests passed"
    else do
      putStrLn $ show (length bad) ++ " FAILURES\n"
      forM_ (zip [1 ..] bad) $ \(i, (a, b)) ->
        putStrLn
          $  "FAILURE "
          ++ show i
          ++ ": "
          ++ a
          ++ "\n"
          ++ showOutput b
          ++ "\n"
      fail
        $  "FAILURE, failed "
        ++ show (length bad)
        ++ " of "
        ++ show total
        ++ " tests"


equivalentTests :: [(String, Property)]
equivalentTests =
  [
    ( "pathSeparator"
    , property $ W.pathSeparator == LW.pathSeparator
    )
    ,
    ( "pathSeparators"
    , property $ W.pathSeparators == LW.pathSeparators
    )
    ,
    ( "isPathSeparator"
    , property $ \p -> W.isPathSeparator p == LW.isPathSeparator p
    )
    ,
    ( "searchPathSeparator"
    , property $ W.searchPathSeparator == LW.searchPathSeparator
    )
    ,
    ( "isSearchPathSeparator"
    , property $ \p -> W.isSearchPathSeparator p == LW.isSearchPathSeparator p
    )
    ,
    ( "extSeparator"
    , property $ W.extSeparator == LW.extSeparator
    )
    ,
    ( "isExtSeparator"
    , property $ \p -> W.isExtSeparator p == LW.isExtSeparator p
    )
    ,
    ( "splitSearchPath"
    , property $ \p -> W.splitSearchPath p == LW.splitSearchPath p
    )
    ,
    ( "splitExtension"
    , property $ \p -> W.splitExtension p == LW.splitExtension p
    )
    ,
    ( "takeExtension"
    , property $ \p -> W.takeExtension p == LW.takeExtension p
    )
    ,
    ( "replaceExtension"
    , property $ \p s -> W.replaceExtension p s == LW.replaceExtension p s
    )
    ,
    ( "dropExtension"
    , property $ \p -> W.dropExtension p == LW.dropExtension p
    )
    ,
    ( "addExtension"
    , property $ \p s -> W.addExtension p s == LW.addExtension p s
    )
    ,
    ( "hasExtension"
    , property $ \p -> W.hasExtension p == LW.hasExtension p
    )
    ,
    ( "splitExtensions"
    , property $ \p -> W.splitExtensions p == LW.splitExtensions p
    )
    ,
    ( "dropExtensions"
    , property $ \p -> W.dropExtensions p == LW.dropExtensions p
    )
    ,
    ( "takeExtensions"
    , property $ \p -> W.takeExtensions p == LW.takeExtensions p
    )
    ,
    ( "replaceExtensions"
    , property $ \p s -> W.replaceExtensions p s == LW.replaceExtensions p s
    )
    ,
    ( "isExtensionOf"
    , property $ \p s -> W.isExtensionOf p s == LW.isExtensionOf p s
    )
    ,
    ( "stripExtension"
    , property $ \p s -> W.stripExtension p s == LW.stripExtension p s
    )
    ,
    ( "splitFileName"
    , property $ \p -> W.splitFileName p == LW.splitFileName p
    )
    ,
    ( "takeFileName"
    , property $ \p -> W.takeFileName p == LW.takeFileName p
    )
    ,
    ( "replaceFileName"
    , property $ \p s -> W.replaceFileName p s == LW.replaceFileName p s
    )
    ,
    ( "dropFileName"
    , property $ \p -> W.dropFileName p == LW.dropFileName p
    )
    ,
    ( "takeBaseName"
    , property $ \p -> W.takeBaseName p == LW.takeBaseName p
    )
    ,
    ( "replaceBaseName"
    , property $ \p s -> W.replaceBaseName p s == LW.replaceBaseName p s
    )
    ,
    ( "takeDirectory"
    , property $ \p -> W.takeDirectory p == LW.takeDirectory p
    )
    ,
    ( "replaceDirectory"
    , property $ \p s -> W.replaceDirectory p s == LW.replaceDirectory p s
    )
    ,
    ( "combine"
    , property $ \p s -> W.combine p s == LW.combine p s
    )
    ,
    ( "splitPath"
    , property $ \p -> W.splitPath p == LW.splitPath p
    )
    ,
    ( "joinPath"
    , property $ \p -> W.joinPath p == LW.joinPath p
    )
    ,
    ( "splitDirectories"
    , property $ \p -> W.splitDirectories p == LW.splitDirectories p
    )
    ,
    ( "splitDirectories"
    , property $ \p -> W.splitDirectories p == LW.splitDirectories p
    )
    ,
    ( "splitDrive"
    , property $ \p -> W.splitDrive p == LW.splitDrive p
    )
    ,
    ( "joinDrive"
    , property $ \p s -> W.joinDrive p s == LW.joinDrive p s
    )
    ,
    ( "takeDrive"
    , property $ \p -> W.takeDrive p == LW.takeDrive p
    )
    ,
    ( "hasDrive"
    , property $ \p -> W.hasDrive p == LW.hasDrive p
    )
    ,
    ( "dropDrive"
    , property $ \p -> W.dropDrive p == LW.dropDrive p
    )
    ,
    ( "isDrive"
    , property $ \p -> W.isDrive p == LW.isDrive p
    )
    ,
    ( "hasTrailingPathSeparator"
    , property $ \p -> W.hasTrailingPathSeparator p == LW.hasTrailingPathSeparator p
    )
    ,
    ( "addTrailingPathSeparator"
    , property $ \p -> W.addTrailingPathSeparator p == LW.addTrailingPathSeparator p
    )
    ,
    ( "dropTrailingPathSeparator"
    , property $ \p -> W.dropTrailingPathSeparator p == LW.dropTrailingPathSeparator p
    )
    ,
    ( "normalise"
    , property $ \p -> W.normalise p == LW.normalise p
    )
    ,
    ( "equalFilePath"
    , property $ \p s -> W.equalFilePath p s == LW.equalFilePath p s
    )
    ,
    ( "makeRelative"
    , property $ \p s -> W.makeRelative p s == LW.makeRelative p s
    )
    ,
    ( "isRelative"
    , property $ \p -> W.isRelative p == LW.isRelative p
    )
    ,
    ( "isAbsolute"
    , property $ \p -> W.isAbsolute p == LW.isAbsolute p
    )
    ,
    ( "isValid"
    , property $ \p -> W.isValid p == LW.isValid p
    )
    ,
    ( "makeValid"
    , property $ \p -> W.makeValid p == LW.makeValid p
    )
  ]













