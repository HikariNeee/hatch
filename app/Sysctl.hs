{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Sysctl(
  sysctlReadString,
  sysctlReadUInt,
  sysctlReadInt,
  getPageSize,
  getHostName,
  _SC_HOST_NAME_MAX,
  _POSIX_LOGIN_NAME_MAX,
  getLogin
) where

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable

foreign import capi unsafe "unistd.h value _SC_HOST_NAME_MAX" _SC_HOST_NAME_MAX :: IO CInt

foreign import capi unsafe "limits.h value _POSIX_LOGIN_NAME_MAX" _POSIX_LOGIN_NAME_MAX :: IO CInt

foreign import capi unsafe "sys/sysctl.h sysctlbyname" sysctl :: CString -> Ptr a -> Ptr CSize -> Ptr b -> CSize -> IO CInt

foreign import capi unsafe "unistd.h sysconf" sysconf :: CInt -> IO CLong

foreign import capi unsafe "unistd.h gethostname" gethostname :: CString -> CSize -> IO CInt

foreign import capi unsafe "unistd.h getpagesize" getpagesize :: IO CInt

foreign import capi unsafe "unistd.h getlogin_r" getlogin_r :: CString -> CSize -> IO CInt

getLogin :: IO String
getLogin = do
  code <- _POSIX_LOGIN_NAME_MAX
  len <- sysconf code
  let buflen = (fromIntegral len) :: CSize
  allocaBytes (fromIntegral len :: Int) $ \buf -> throwErrnoIfMinus1_ "getlogin_r" (getlogin_r buf buflen) >> peekCAString buf

getHostNameLen :: IO Int
getHostNameLen = do
 len <- _SC_HOST_NAME_MAX
 maxlen <- sysconf len
 return . fromIntegral . (+1) $ maxlen

getPageSize :: IO Int
getPageSize = fmap fromIntegral getpagesize

getHostName :: IO String
getHostName = do
  len <- getHostNameLen
  allocaBytes len $ \z -> throwErrnoIfMinus1_ "gethostname" (gethostname z (fromIntegral len :: CSize)) >> peekCAString z

sysctlRead :: CString -> Ptr a -> CSize -> (Ptr a -> CSize -> IO b) -> IO b
sysctlRead name buf size f = alloca $ \sizePtr -> do
   poke sizePtr size
   throwErrnoIfMinus1_ "sysctl" (sysctl name buf sizePtr nullPtr 0)
   realSize <- peek sizePtr
   f buf realSize

sysctlGetSize :: CString -> IO CSize
sysctlGetSize name = sysctlRead name nullPtr 0 (const return)

sysctlPeek :: forall a. (Storable a) => String -> IO a
sysctlPeek name = withCString name $ \x -> do
  alloca $ \(buf :: Ptr a) ->
   sysctlRead x buf (fromIntegral (sizeOf (undefined :: a))) (const . peek)

sysctlRead' :: CString -> (Ptr a -> CSize -> IO b) -> IO b
sysctlRead' name f = do
 bufSize <- sysctlGetSize name
 allocaBytes (fromIntegral bufSize) $ \buf -> sysctlRead name buf bufSize f

sysctlPeek' :: String -> IO B.ByteString
sysctlPeek' name = withCString name $ \sysctlname -> do
   C.pack <$> sysctlRead' sysctlname (const . peekCAString)


sysctlReadInt :: String -> IO Int
sysctlReadInt = sysctlPeek

sysctlReadString :: String -> IO B.ByteString
sysctlReadString = sysctlPeek'

sysctlReadUInt :: String -> IO Int
sysctlReadUInt name = fromIntegral <$> (sysctlPeek name :: IO CUInt)
