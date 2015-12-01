{-# LINE 1 "Database/HDBC/MySQL/RTS.hsc" #-}
{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
{-# LINE 2 "Database/HDBC/MySQL/RTS.hsc" #-}

module Database.HDBC.MySQL.RTS (withRTSSignalsBlocked) where

import Control.Concurrent (runInBoundThread)
import Control.Exception (finally)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))


{-# LINE 13 "Database/HDBC/MySQL/RTS.hsc" #-}

-- | Execute an 'IO' action with signals used by GHC's runtime signals
-- blocked.  The @mysqlclient@ C library does not correctly restart
-- system calls if they are interrupted by signals, so many MySQL API
-- calls can unexpectedly fail when called from a Haskell application.
-- This is most likely to occur if you are linking against GHC's
-- threaded runtime (using the @-threaded@ option).
--
-- This function blocks @SIGALRM@ and @SIGVTALRM@, runs your action,
-- then unblocks those signals.  If you have a series of HDBC calls
-- that may block for a period of time, it may be wise to wrap them in
-- this action.  Blocking and unblocking signals is cheap, but not
-- free.
--
-- Here is an example of an exception that could be avoided by
-- temporarily blocking GHC's runtime signals:
--
-- >  SqlError {
-- >    seState = "", 
-- >    seNativeError = 2003, 
-- >    seErrorMsg = "Can't connect to MySQL server on 'localhost' (4)"
-- >  }
withRTSSignalsBlocked :: IO a -> IO a
withRTSSignalsBlocked act = runInBoundThread . alloca $ \set -> do
  sigemptyset set
  sigaddset set (14)
{-# LINE 39 "Database/HDBC/MySQL/RTS.hsc" #-}
  sigaddset set (26)
{-# LINE 40 "Database/HDBC/MySQL/RTS.hsc" #-}
  pthread_sigmask (0) set nullPtr
{-# LINE 41 "Database/HDBC/MySQL/RTS.hsc" #-}
  act `finally` pthread_sigmask (1) set nullPtr
{-# LINE 42 "Database/HDBC/MySQL/RTS.hsc" #-}

data SigSet

instance Storable SigSet where
    sizeOf    _ = (128)
{-# LINE 47 "Database/HDBC/MySQL/RTS.hsc" #-}
    alignment _ = alignment (undefined :: Ptr CInt)

foreign import ccall unsafe "signal.h sigaddset" sigaddset
    :: Ptr SigSet -> CInt -> IO ()

foreign import ccall unsafe "signal.h sigemptyset" sigemptyset
    :: Ptr SigSet -> IO ()

foreign import ccall unsafe "signal.h pthread_sigmask" pthread_sigmask
    :: CInt -> Ptr SigSet -> Ptr SigSet -> IO ()
