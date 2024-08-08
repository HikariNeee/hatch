module Common where


import System.Environment (getEnv)
import Data.Foldable (find)
import System.Process.Typed (readProcessStdout,ExitCode(..),shell)
import System.Info (os)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Sysctl as S (getHostName,sysctlReadString)
import BSD (getCPUBSD,getMemBSD)
import Control.Monad ((<$!>))

mapCPU :: [(B.ByteString, IO B.ByteString)]
mapCPU = [("freebsd", getCPUBSD)]

getSection :: FilePath -> B.ByteString -> IO B.ByteString
getSection x y = C.readFile x >>= f
 where m = find (y `C.isPrefixOf`) . C.lines
       n = C.length y
       f q = if | Just text <- m q -> return . C.drop n $ text
                | otherwise -> fail "cannot find name."

getUserName :: IO String
getUserName = getEnv "USER"

getUser :: IO B.ByteString
getUser = C.pack <$!> liftA2 ((<>) . (<>) "\x1b[36mUser: \x1b[0m" . flip (<>) "@") getUserName S.getHostName

getShell :: IO B.ByteString
getShell = (C.pack . (<>) "\x1b[31;1mShell: \x1b[0m") <$!> getEnv "SHELL"

getMemory :: IO B.ByteString
getMemory = (<>) "\x1b[34mMemory: \x1b[0m" <$!> getMemBSD

getOSRel :: B.ByteString -> IO B.ByteString
getOSRel = getSection "/etc/os-release"

getPrettyName :: IO B.ByteString
getPrettyName = (<>) "\x1b[32mOS: \x1b[0m" . flip (<>) " " <$!> getName 

getVersion ::  IO B.ByteString
getVersion = (<>) "\x1b[38mVersion: \x1b[0m" <$!> S.sysctlReadString "kern.osrelease"

getName :: IO B.ByteString
getName = S.sysctlReadString "kern.ostype"

getFiglet :: IO B.ByteString
getFiglet = do
 x <- getName

 (err,stdout) <- readProcessStdout $ shell $ "figlet " ++ (C.unpack x)
 if err == ExitSuccess then
   return stdout
 else
   fail "Could you create figlet."

getCPU :: IO B.ByteString
getCPU = if | Just y <- x -> (<>) "\x1b[33mCPU: \x1b[0m " <$> y
            | otherwise   -> fail "cannot determine CPU. OS.unsupported"
            
 where x = lookup (C.pack $ os) mapCPU


getDisplayServer :: IO B.ByteString
getDisplayServer = (<>) "\x1b[35mServer: \x1b[0m". C.pack <$!> getEnv "XDG_SESSION_TYPE"

