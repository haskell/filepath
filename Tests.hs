
module Main where

import System.FilePath




extension = 
    [(getExtension "file.txt", ".txt")
    ,(getExtension "file", "")
    ,(getExtension "file/file.txt", ".txt")
    ,(getExtension "file.txt/boris", "")
    ,(getExtension "file.txt/boris.ext", ".ext")
    ,(getExtension "file/path.txt.bob.fred", ".fred")
    
    ,(setExtension "file.txt" ".bob", "file.bob")
    ,(setExtension "file.txt" "bob", "file.bob")
    ,(setExtension "file" ".bob", "file.bob")
    ,(setExtension "file.txt" "", "file")
    ,(setExtension "file.fred.bob" "txt", "file.fred.txt")
    
    ,(dropExtension "file.txt", "file")
    ,(dropExtension "file.txt/file.bob", "file.txt/file")
    ,(dropExtension "file/file", "file/file")
    ]



main = print $ all (uncurry (==)) extension
