
module Main where

-- import the relevant modules under test
import System.FilePath
import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P

import Test.QuickCheck

-- framework support for the various tests
data Test a = Test Int [Int]
              deriving Show

instance Monad Test where
    return _ = error "Not implemented"
    _ >>= _  = error "Not implemented"
    (Test c1 x1) >> (Test c2 x2) = Test (c1+c2) (x1 ++ map (+c1) x2)
    

(===) :: Eq a => a -> a -> Test ()
a === b = bool $ a == b

a =/= b = bool $ a /= b

with :: String -> Test () -> Bool
with msg (Test _ []) = True
with msg (Test _ fs) = error $ "Failed tests using " ++ show msg ++ ", on " ++ show fs

bool :: Bool -> Test ()
bool True  = Test 1 []
bool False = Test 1 [1]

data QFilePath = QFilePath FilePath
                 deriving Show

instance Arbitrary QFilePath where
    arbitrary = vector 25 >>= return . QFilePath

instance Arbitrary Char where
    arbitrary = oneof $ map return "./:\\abcd"


tests = do
    -- tests of the basic operations
    W.pathSeparator === '\\'
    P.pathSeparator === '/'
    bool $ all W.isPathSeparator "\\/"
    W.fileSeparator === ';'
    P.fileSeparator === ':'
    extSeparator === '.'

    splitExtension "file.txt" === ("file",".txt")
    splitExtension "file" === ("file","")
    splitExtension "file/file.txt" === ("file/file",".txt")
    splitExtension "file.txt/boris" === ("file.txt/boris","")
    splitExtension "file.txt/boris.ext" === ("file.txt/boris",".ext")
    splitExtension "file/path.txt.bob.fred" === ("file/path.txt.bob",".fred")
    splitExtension "file/path.txt/" === ("file/path.txt/","")
    
    setExtension "file.txt" ".bob" === "file.bob"
    setExtension "file.txt" "bob" === "file.bob"
    setExtension "file" ".bob" === "file.bob"
    setExtension "file.txt" "" === "file"
    setExtension "file.fred.bob" "txt" === "file.fred.txt"
    
    addExtension "file.txt" "bib" === "file.txt.bib"
    addExtension "file." ".bib" === "file..bib"
    addExtension "file" ".bib" === "file.bib"
    
    W.getDrive "file" === ""
    W.getDrive "c:/file" === "c:"
    W.getDrive "\\\\shared\\test" === "\\\\shared"
    P.getDrive "/test" === ""
    P.getDrive "file" === ""


main = if null failed then
            putStrLn $ "All tests passed (" ++ show count ++ ")"
       else
            putStrLn $ "FAILURES: " ++ show failed
    where (Test count failed) = tests


simpleJoin (a,b) = a ++ b


extTests (QFilePath x) = with x $ do
    -- extension tests
    uncurry joinExtension (splitExtension x) === x
    simpleJoin (splitExtension x) === x
    getExtension x === snd (splitExtension x)
    dropExtension x === fst (splitExtension x)
    null (getExtension x) =/= hasExtension x
    getExtension (addExtension x "ext") === ".ext"
    getExtension (setExtension x "ext") === ".ext"
