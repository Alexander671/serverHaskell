{-# LANGUAGE DeriveGeneric #-}


module Logging.File where
import System.IO (openFile, IOMode(ReadWriteMode),hGetContents,hClose)
import System.Directory (doesFileExist, renameFile)
import Data.Time.LocalTime (getZonedTime)

checkLenFile :: IO Int
checkLenFile = do
     bool <- doesFileExist "log.log"
     if bool
     then readFile "log.log" >>= return . length
     else return 0

newFile :: IO ()
newFile = do
    len <- checkLenFile
    if (len > 10000)
    then do 
          timeNow <- getZonedTime
          renameFile "log.log"  $ "log" ++ show timeNow ++ ".log"
          writeFile "log.log" ""
    else putStrLn $ show len

        