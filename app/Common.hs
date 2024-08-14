module Common(
   getUser,
   getShell,
   getMemory,
   getFiglet,
   getCPU,
   getPrettyName,
   getDisplayServer,
   getVersion,
   getName
)
 where


import           BSD                        (getCPUBSD, getMemBSD)
import           Control.Monad              ((<$!>))
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Sysctl                     as S (getHostName, getLogin,
                                                  sysctlReadString)
import           System.Environment         (getEnv)
import           System.Process


{-# NOINLINE getUserName #-}
getUserName :: IO String
getUserName = S.getLogin


{-# NOINLINE getUser #-}
getUser :: IO B.ByteString
getUser = C.pack <$!> liftA2 ((<>) . (<>) "\x1b[36mUser: \x1b[0m" . flip (<>) "@") getUserName S.getHostName


{-# NOINLINE getShell #-}
getShell :: IO B.ByteString
getShell = (C.pack . (<>) "\x1b[31;1mShell: \x1b[0m") <$!> getEnv "SHELL"


{-# NOINLINE getMemory #-}
getMemory :: IO B.ByteString
getMemory = (<>) "\x1b[34mMemory: \x1b[0m" <$!> getMemBSD


{-# NOINLINE getPrettyName #-}
getPrettyName :: IO B.ByteString
getPrettyName = (<>) "\x1b[32mOS: \x1b[0m" . flip (<>) " " <$!> getName


{-# NOINLINE getVersion #-}
getVersion ::  IO B.ByteString
getVersion = (<>) "\x1b[38mVersion: \x1b[0m" <$!> S.sysctlReadString "kern.osrelease"


{-# NOINLINE getName #-}
getName :: IO B.ByteString
getName = S.sysctlReadString "kern.ostype"

getFiglet :: IO B.ByteString
getFiglet = do
 os <- getName
 stdout <- readProcess "figlet" [C.unpack os] []
 return . C.pack $ stdout


{-# NOINLINE getCPU #-}
getCPU :: IO B.ByteString
getCPU = (<>) "\x1b[36mCPU: \x1b[0m" <$!> getCPUBSD


{-# NOINLINE getDisplayServer #-}
getDisplayServer :: IO B.ByteString
getDisplayServer = (<>) "\x1b[35mServer: \x1b[0m". C.pack <$!> getEnv "XDG_SESSION_TYPE"
