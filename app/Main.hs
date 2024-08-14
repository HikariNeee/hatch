module Main where

import qualified Common                     as CE
import qualified Data.ByteString.Lazy.Char8 as C
import qualified System.Directory           as D

main :: IO ()
main = do
  prettyname  <- CE.getPrettyName
  name        <- CE.getName
  cpumodel    <- CE.getCPU
  memory      <- CE.getMemory
  username    <- CE.getUser
  displaysrv  <- CE.getDisplayServer
  shell       <- CE.getShell
  version     <- CE.getVersion
  configDir   <- D.getXdgDirectory D.XdgConfig "hatch/sixels/"
  sixel       <- C.readFile $ C.unpack $ C.pack configDir <> (name <> C.pack ".sixel")
  C.putStrLn $ (<>) (C.replicate 10 ' ') sixel
  C.putStrLn $ C.unlines $ (<>) (C.replicate 4 ' ')  <$> [prettyname,version,username,cpumodel,memory,shell,displaysrv]
