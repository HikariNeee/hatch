module BSD where


import System.Process.Typed (shell,readProcessStdout,ExitCode(..))
import Data.Foldable (find)
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Sysctl as S
import Control.Monad (liftM)

toDouble :: Int -> Double
toDouble = fromIntegral

memToMB :: Int -> Int
memToMB =  round . (flip (/) 1048576) . toDouble

getCPUBSD :: IO B.ByteString
getCPUBSD = liftM (C.unwords . C.words . C.drop 9) $ S.sysctlReadString "hw.model"

getTotalMemBSD :: IO B.ByteString
getTotalMemBSD = liftM (C.pack . show . memToMB) $ S.sysctlReadInt "hw.physmem"

getCurrentMemBSD :: IO B.ByteString
getCurrentMemBSD = do
 pagesize <- S.getPageSize
 usedmem  <- S.sysctlReadUInt "vm.stats.vm.v_active_count"
 return . C.pack . show . memToMB . (*) pagesize $ usedmem

getMemBSD :: IO B.ByteString
getMemBSD = do
    x <- getTotalMemBSD
    y <- getCurrentMemBSD
    return $ (y <> "MB/") <> (x <> "MB")
   