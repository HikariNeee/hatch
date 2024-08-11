{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeSynonymInstances     #-}

module Sysctl where

#ifdef freebsd_HOST_OS
#define _SC_HOST_NAME_MAX 72
#elif openbsd_HOST_OS
#define _SC_HOST_NAME_MAX 33
#endif

import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Int                   as I (Int32)
import           Foreign.C
import           Foreign.Marshal
import           Foreign.Ptr
import           Foreign.Storable


foreign import capi unsafe "sys/sysctl.h sysctlbyname" sysctl :: CString -> Ptr a -> Ptr CSize -> Ptr b -> CSize -> IO CInt

foreign import capi unsafe "unistd.h sysconf" sysconf :: CInt -> IO CLong

foreign import capi unsafe "unistd.h gethostname" gethostname :: CString -> CSize -> IO CInt

foreign import capi unsafe "unistd.h getpagesize" getpagesize :: IO CInt

getHostNameLen :: IO Int
getHostNameLen = fmap (fromIntegral . (+1)) $ sysconf $ CInt (_SC_HOST_NAME_MAX :: I.Int32)

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
sysctlPeek name = do
  x <- newCAString name
  alloca $ \(buf :: Ptr a) ->
   sysctlRead x buf (fromIntegral (sizeOf (undefined :: a))) (const . peek)

sysctlRead' :: CString -> (Ptr a -> CSize -> IO b) -> IO b
sysctlRead' name f = do
 bufSize <- sysctlGetSize name
 allocaBytes (fromIntegral bufSize) $ \buf -> sysctlRead name buf bufSize f

sysctlPeek' :: String -> IO B.ByteString
sysctlPeek' name = do
   sysctlname <- newCAString name
   C.pack <$> sysctlRead' sysctlname (const . peekCAString)


sysctlReadInt :: String -> IO Int
sysctlReadInt = sysctlPeek

sysctlReadString :: String -> IO B.ByteString
sysctlReadString = sysctlPeek'

sysctlReadUInt :: String -> IO Int
sysctlReadUInt name = fmap fromIntegral (sysctlPeek name :: IO CUInt)
