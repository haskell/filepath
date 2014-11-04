
import System.Exit
import System.Process

main = do
    ExitSuccess <- system "runhaskell Generate"
    return ()

