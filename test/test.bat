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
echo Hpc compilation here
goto end


:help
echo FilePath tester script
echo   test            Run the tests using runhaskell
echo   test ghci       Run the tests using ghci
echo   test hpc        Run the tests using hpc
goto end

:end
