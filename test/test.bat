@echo off

if "%1" == "help" goto help

echo Generating test script
runhaskell GenTests

if "%1" == "hpc" goto hpc
if "%1" == "ghci" goto ghci

runhaskell -i.. -cpp -DTESTING FilePath_Test.hs
goto end


:ghci
ghci -i.. -cpp -DTESTING FilePath_Test.hs
goto end


:hpc
mkdir hpc 2> nul
mkdir hpc\System 2> nul
mkdir hpc\System\FilePath 2> nul
type AutoTest.hs > hpc\AutoTest.hs
type FilePath_Test.hs > hpc\FilePath_Test.hs
cpphs --noline -DTESTING ..\System\FilePath\Posix.hs > hpc\System\FilePath\Posix.hs
cpphs --noline -DTESTING ..\System\FilePath\Windows.hs > hpc\System\FilePath\Windows.hs
pushd hpc
ghc FilePath_Test.hs -o test --make -fhpc
test
hpc markup test.tix --exclude=AutoTest --exclude=Main
hpc report test.tix --exclude=AutoTest --exclude=Main
popd
goto end


:help
echo FilePath tester script
echo   test            Run the tests using runhaskell
echo   test ghci       Run the tests using ghci
echo   test hpc        Run the tests using hpc
goto end

:end
