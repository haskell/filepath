ghc System\FilePath\Posix.hs -cpp -DTESTING
ghc System\FilePath\Windows.hs -cpp -DTESTING
runhaskell GenTests
runhaskell FilePath_Test
