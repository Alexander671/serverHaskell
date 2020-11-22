{-# LANGUAGE DeriveGeneric #-}


module Logging.File where
import System.Directory (doesFileExist, renameFile)
import Data.Time.LocalTime (getZonedTime)

checkLenFile :: IO Int
checkLenFile = do
     bool <- doesFileExist "app/Logging/log.log"
     if bool
     then readFile "app/Logging/log.log" >>= return . length
     else return 0

newFile :: IO ()
newFile = do
    len <- checkLenFile
    if (len > 50000)
    then do 
          timeNow <- getZonedTime
          renameFile "app/Logging/log.log"  $ "log" ++ show timeNow ++ ".log"
          writeFile "app/Logging/log.log" ""
    else putStrLn $ show len

        