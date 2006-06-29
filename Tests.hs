
module Main where

-- import the relevant modules under test
import System.FilePath
import qualified System.FilePath.Windows as W
import qualified System.FilePath.Posix as P

import Test.QuickCheck
import Data.List

-- framework support for the various tests
data Test a = Test [Answer]
              deriving Show

data Answer = Answer Bool
            | Section String
              deriving Show

instance Monad Test where
    return _ = error "Not implemented"
    _ >>= _  = error "Not implemented"
    
    (Test x1) >> (Test x2) = (Test (x1 ++ x2))
    

(===) :: Eq a => a -> a -> Test ()
a === b = bool $ a == b

a =/= b = bool $ a /= b

a =~= b = bool $ a `equalFilePath` b

evalTests :: Test () -> (Int, String)
evalTests (Test rs) = (length (filter isAnswer rs), disp $ f "" 1 rs)
    where
        isAnswer (Answer _) = True
        isAnswer _ = False
        
        disp [] = []
        disp xs = concatMap g $ groupBy (\a b -> fst a == fst b) xs

        g xs = fst (head xs) ++ " " ++ show (map snd xs) ++ " "
    
        f sname snum [] = []
        f sname snum (Section name:xs) = f name 0 xs
        f sname snum (Answer b:xs) = [(sname,snum+1) | not b] ++ f sname (snum+1) xs


with :: String -> Test () -> Bool
with msg test = if null fs then True else error $ "Failed tests using " ++ show msg ++ ", in " ++ fs
    where (n,fs) = evalTests test

bool :: Bool -> Test ()
bool b = Test [Answer b]

section :: String -> Test ()
section name = Test [Section name]

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
    
    section "drive"
    W.splitDrive "file" === ("","file")
    W.splitDrive "c:/file" === ("","c:/file")
    W.splitDrive "c:\\file" === ("c:\\","file")
    W.splitDrive "\\\\shared\\test" === ("\\\\shared\\","test")
    W.splitDrive "\\\\shared" === ("\\\\shared","")
    P.splitDrive "/test" === ("/","test")
    P.splitDrive "test/file" === ("","test/file")
    P.splitDrive "file" === ("","file")
    
    section "filename"
    splitFileName "file/bob.txt" === ("file/", "bob.txt")
    splitFileName "file/" === ("file/", "")
    splitFileName "bob" === ("", "bob")
    P.splitFileName "/" === ("/","")
    W.splitFileName "c:" === ("c:","")
    splitPath "test//item/" === ["test//","item/"]
    splitPath "test/item/file" === ["test/","item/","file"]
    W.splitPath "c:\\test\\path" === ["c:\\","test\\","path"]
    P.splitPath "/file/test" === ["/","file/","test"]


main = do
            if null failed then
                putStrLn $ "All static tests passed (" ++ show count ++ ")"
             else
                error $ "FAILURES: " ++ show failed
            quickCheck quickTests
    where (count, failed) = evalTests tests


simpleJoin (a,b) = a ++ b


quickTests (QFilePath x) = with x $ do

    section "extension"
    uncurry joinExtension (splitExtension x) === x
    simpleJoin (splitExtension x) === x
    getExtension x === snd (splitExtension x)
    dropExtension x === fst (splitExtension x)
    null (getExtension x) =/= hasExtension x
    getExtension (addExtension x "ext") === ".ext"
    getExtension (setExtension x "ext") === ".ext"

    section "file"
    uncurry joinFileName (splitFileName x) === x
    simpleJoin (splitFileName x) === x
    getFileName x === snd (splitFileName x)
    dropFileName x === fst (splitFileName x)
    setDirectory x (getDirectory x) =~= x
    setFileName x (getFileName x) === x
    addFileName (getDirectory x) (getFileName x) =~= x
    getFileName (setFileName x "fred") === "fred"
    getFileName (addFileName x "fred") === "fred"
    
    section "file path"
    joinPath (splitPath x) === x
    concat (splitPath x) === x

    section "drive"
    uncurry joinDrive (splitDrive x) === x
    simpleJoin (splitDrive x) === x
    getDrive x === fst (splitDrive x)
    dropDrive x === snd (splitDrive x)
    setDrive x (getDrive x) === x
    
