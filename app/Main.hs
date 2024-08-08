module Main where

import Common
import Data.Foldable(traverse_)
import qualified Data.ByteString.Lazy.Char8 as C

main :: IO ()
main = do
  x <- getPrettyName
--  y <- getFiglet
  z <- getCPU
  a <- getMemory
  b <- getUser
  c <- getDisplayServer
  d <- getShell
  m <- getVersion
 -- C.putStrLn y
  traverse_ C.putStrLn [x,m,b,d,z,a,c]
