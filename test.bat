if not "%1" == "" goto ghci

del System\FilePath\Posix.o
del System\FilePath\Windows.o
ghc System\FilePath\Posix.hs -c -cpp -DTESTING
ghc System\FilePath\Windows.hs -c -cpp -DTESTING
runhaskell GenTests
runhaskell FilePath_Test
goto exit

:ghci
ghci FilePath_Test

:exit
