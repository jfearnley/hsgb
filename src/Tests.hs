
import Control.Monad
import Control.Monad.Loops
import System.FilePath
import System.Console.Pretty
import System.Directory
import System.Exit
import Data.List

import Types
import GB
import CpuBasic
import Memory
import Emulation


data TestResult = Passed | Failed String | Timeout deriving Show

executeTest :: GBState s TestResult
executeTest = do
    let testStatus = (/=0) <$> readMem 0xC800
        cycleCount = (>= 4000000) <$> askRef cycles
    untilM_ step (liftM2 (||) testStatus cycleCount)
    stat   <- readMem 0xC800
    serial <- askRef serialBuffer
    return $ case stat of 0x00 -> Timeout
                          0x01 -> Passed
                          0x02 -> Failed serial
                          _    -> Failed "corrupt memory"

runTest :: FilePath -> IO TestResult
runTest file = do
    gb <- loadRom file
    return $ evalState executeTest gb
    
runTests :: FilePath -> IO [(FilePath, TestResult)]
runTests dir = do
    files   <- sort . filter (isExtensionOf "gb") <$> listDirectory dir
    results <- mapM runTest $ map (dir </>) files
    return $ zip files results


printResult :: (FilePath, TestResult) -> String
printResult (test, result) = 
    color c (test ++ ": ") ++ case result of Passed     -> "pass"
                                             Failed err -> err
                                             Timeout    -> "timeout"
        where c = case result of Passed -> Green
                                 _      -> Red

failed :: (FilePath, TestResult) -> Bool
failed (_, Passed) = False
failed (_, _)      = True

main :: IO ()
main = do
    putStr "\n"
    results <- runTests "tests"
    putStrLn . unlines $ map printResult results
    if any failed results then exitFailure else exitSuccess
