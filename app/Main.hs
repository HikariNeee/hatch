module Main where

import qualified Common                     as CE
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
  x <- CE.getPrettyName
  y <- CE.getFiglet
  z <- CE.getCPU
  a <- CE.getMemory
  b <- CE.getUser
  c <- CE.getDisplayServer
  d <- CE.getShell
  m <- CE.getVersion
  C.putStrLn y
  C.putStrLn $ C.intercalate "\n" [x,m,b,d,z,a,c]
