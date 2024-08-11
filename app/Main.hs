module Main where

import qualified Common                      as CE
import qualified Data.ByteString.Lazy.Char8  as C

main :: IO ()
main = do
  prettyname  <- CE.getPrettyName
  figlet      <- CE.getFiglet
  cpumodel    <- CE.getCPU
  memory      <- CE.getMemory
  username    <- CE.getUser
  displaysrv  <- CE.getDisplayServer
  shell       <- CE.getShell
  version     <- CE.getVersion
  C.putStrLn $ figlet
  C.putStrLn $ C.unlines $ (<>) (C.replicate 4 ' ')  <$> [prettyname,version,username,shell,cpumodel,memory,displaysrv]
