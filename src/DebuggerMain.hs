
import Debugger
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
    then 
        putStrLn "usage: debugger [romfile]"
    else do
        state <- loadState $ head args
        case state of
            Nothing -> putStrLn $ "failed to load " ++ head args
            Just st -> repl st
