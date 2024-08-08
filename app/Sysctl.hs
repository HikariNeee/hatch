{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI                  #-}
{-# LANGUAGE TypeSynonymInstances     #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE CPP                      #-}

module Sysctl where

#ifdef freebsd_HOST_OS
#define _SC_HOST_NAME_MAX 72
#elif openbsd_HOST_OS
#define _SC_HOST_NAME_MAX 33
#endif

import Foreign.Ptr
import Foreign.C
import Foreign.Storable
import Foreign.Marshal
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.Int as I (Int32)
import Control.Monad (liftM)

foreign import capi unsafe "sys/sysctl.h sysctlbyname" sysctl :: CString -> Ptr a -> Ptr CSize -> Ptr b -> CSize -> IO CInt

foreign import capi unsafe "unistd.h sysconf" sysconf :: CInt -> IO CLong

foreign import capi unsafe "unistd.h gethostname" gethostname :: CString -> CSize -> IO CInt

foreign import capi unsafe "unistd.h getpagesize" getpagesize :: IO CInt

getHostNameLen :: IO Int
getHostNameLen = fmap (fromIntegral . (+1)) $ sysconf $ CInt (_SC_HOST_NAME_MAX :: I.Int32)

getPageSize :: IO Int
getPageSize = fmap fromIntegral getpagesize

getHostName :: IO String
getHostName = alloca $ \buf -> do
  x <- getHostNameLen
  poke buf 256
  let y = (fromIntegral x) :: CSize
  throwErrnoIfMinus1_ "gethostname" (gethostname buf y)
  peekCAString buf
  

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
sysctlPeek' y = do 
   x <- newCAString y
   C.pack <$> sysctlRead' x (const . peekCAString) 


sysctlReadInt :: String -> IO Int
sysctlReadInt = sysctlPeek 

sysctlReadString :: String -> IO B.ByteString
sysctlReadString = sysctlPeek'

sysctlReadUInt :: String -> IO Int
sysctlReadUInt x = fmap fromIntegral (sysctlPeek x :: IO CUInt)
