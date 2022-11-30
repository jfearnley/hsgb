import qualified Data.Set as Set
import Data.Functor.Identity 
import Control.Monad.Reader
import System.Environment
import Data.Store
import qualified Data.Vector.Unboxed as VU

import Types
import GB
import Frontend
import Emulation
import Debugger






runFrames :: Int -> PureGB -> Frontend -> IO (PureGB)
runFrames n gb fe 
    | n <= 0 = return gb
    | otherwise = do
        let skip = 0
            (newGB, output) = emulate gb skip
        newKeys <- doFrontend fe newGB output
        when (Set.member KeyDebug newKeys) $ launchDebugger newGB
        let retGB = newGB {keys = Identity newKeys}
        if (Set.notMember KeyQuit newKeys) 
            then runFrames (n-skip-1) retGB fe
            else return retGB

mainLoop :: PureGB -> Frontend -> IO ()
mainLoop gb fe = do
    newGB <- runFrames 60 gb fe
    fe' <- everySecond fe
    let keyset = runIdentity . keys $ newGB
    when (Set.notMember KeyQuit keyset) $
        mainLoop newGB fe'



main :: IO ()
main = do
    args <- getArgs
    if (length args /= 1) 
    then do 
        putStrLn "usage: hsgb [path to rom]"
    else do
        mgb <- loadRom (args !! 0)
        case mgb of Nothing -> putStrLn "Error: only MBC1 roms are supported"
                    Just gb -> do printRomStats gb
                                  fe <- initFrontend
                                  mainLoop gb fe
                                  quitFrontend fe
