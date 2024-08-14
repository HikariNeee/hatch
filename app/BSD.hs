module BSD(
  getCPUBSD,
  getMemBSD
) where

import           Control.Monad              (liftM)
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Sysctl                     as S

{-# INLINE toDouble #-}
toDouble :: Int -> Double
toDouble = fromIntegral

{-# INLINE memToMB #-}
memToMB :: Int -> Int
memToMB =  round . (*)  9.53674316406e-7 . toDouble


{-# NOINLINE getCPUBSD #-}
getCPUBSD :: IO B.ByteString
getCPUBSD = liftM (C.unwords . C.words) $ S.sysctlReadString "hw.model"


{-# NOINLINE getTotalMemBSD #-}
getTotalMemBSD :: IO B.ByteString
getTotalMemBSD = liftM (C.pack . show . memToMB) $ S.sysctlReadInt "hw.physmem"


{-# NOINLINE getCurrentMemBSD #-}
getCurrentMemBSD :: IO B.ByteString
getCurrentMemBSD = do
 pagesize <- S.getPageSize
 usedmem  <- S.sysctlReadUInt "vm.stats.vm.v_active_count"
 return . C.pack . show . memToMB . (*) pagesize $ usedmem

{-# NOINLINE getMemBSD #-}
getMemBSD :: IO B.ByteString
getMemBSD = do
    totalmem <- getTotalMemBSD
    currentmem <- getCurrentMemBSD
    return $ (currentmem <> "MB/") <> (totalmem <> "MB")

