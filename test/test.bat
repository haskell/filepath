
set cmd=runhaskell
if not "%1" == "" set cmd=ghci

runhaskell GenTests
%cmd% -i.. -cpp -DTESTING FilePath_Test.hs

