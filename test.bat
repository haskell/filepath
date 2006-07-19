ghc System\FilePath\Posix.hs -c -cpp -DTESTING
ghc System\FilePath\Windows.hs -c -cpp -DTESTING
runhaskell GenTests
runhaskell FilePath_Test
