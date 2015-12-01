{-# LINE 1 "Database/HDBC/MySQL/Connection.hsc" #-}
{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, ScopedTypeVariables #-}
{-# LINE 2 "Database/HDBC/MySQL/Connection.hsc" #-}

module Database.HDBC.MySQL.Connection
    (
      connectMySQL
    , MySQLConnectInfo(..)
    , defaultMySQLConnectInfo
    , Connection
    ) where

import Control.Exception
import Control.Monad
import Foreign
import Foreign.C
import qualified Foreign.Concurrent
import qualified Data.ByteString as B
import Data.ByteString.UTF8 (fromString, toString)
import Data.List (isPrefixOf)
import Data.Time
import Data.Time.Clock.POSIX

import qualified Database.HDBC.Types as Types
import Database.HDBC.ColTypes as ColTypes
import Database.HDBC (throwSqlError)


{-# LINE 27 "Database/HDBC/MySQL/Connection.hsc" #-}

{- | Connection information to use with connectMySQL.

     You must either supply a host and port, or the full path to a
     Unix socket.

-}
data MySQLConnectInfo = MySQLConnectInfo
    { -- | The server's hostname; e.g., @\"db1.example.com\"@
      mysqlHost       :: String
      -- | The MySQL username to use for login; e.g., @\"scott\"@
    , mysqlUser       :: String
      -- | The MySQL password to use for login; e.g., @\"tiger\"@
    , mysqlPassword   :: String
      -- | the \"default\" database name; e.g., @\"emp\"@
    , mysqlDatabase   :: String
      -- | The port on which to connect to the server; e.g., @3306@
    , mysqlPort       :: Int
      -- | The absolute path of the server's Unix socket; e.g., @\"\/var\/lib\/mysql.sock\"@
    , mysqlUnixSocket :: String
      -- | The group name in my.cnf from which it reads options; e.g., @\"test\"@
    , mysqlGroup      :: Maybe String
    }

{- | Typical connection information, meant to be overridden partially,
     for example:

     > connectMySQL defaultMySQLConnectInfo { mysqlHost = "db1" }

     In particular, the default values are @\"127.0.0.1\"@ as the
     host, @3306@ as the port, @\"root\"@ as the user, no password,
     and @\"test\"@ as the default database.

-}
defaultMySQLConnectInfo :: MySQLConnectInfo
defaultMySQLConnectInfo = MySQLConnectInfo "127.0.0.1" "root" "" "test" 3306 "" Nothing

data Connection = Connection
    { disconnect :: IO ()
    , commit :: IO ()
    , rollback :: IO ()
    , run :: String -> [Types.SqlValue] -> IO Integer
    , runRaw :: String -> IO ()
    , prepare :: String -> IO Types.Statement
    , clone :: IO Connection
    , hdbcDriverName :: String
    , hdbcClientVer :: String
    , proxiedClientName :: String
    , proxiedClientVer :: String
    , dbServerVer :: String
    , dbTransactionSupport :: Bool
    , getTables :: IO [String]
    , describeTable :: String -> IO [(String, ColTypes.SqlColDesc)]
    }

instance Types.IConnection Connection where
  disconnect           = disconnect
  commit               = commit
  rollback             = rollback
  run                  = run
  runRaw               = runRaw
  prepare              = prepare
  clone                = clone
  hdbcDriverName       = hdbcDriverName
  hdbcClientVer        = hdbcClientVer
  proxiedClientName    = proxiedClientName
  proxiedClientVer     = proxiedClientVer
  dbServerVer          = dbServerVer
  dbTransactionSupport = dbTransactionSupport
  getTables            = getTables
  describeTable        = describeTable

-- The "real" connection to the MySQL server.  Wraps mysql.h's MYSQL
-- struct.  We don't ever need to look inside it.
data MYSQL

{- | Connects to a MySQL database using the specified
     connection information. -}
connectMySQL :: MySQLConnectInfo -> IO Connection
connectMySQL info = do
  mysql_ <- mysql_init nullPtr
  when (mysql_ == nullPtr) (error "mysql_init failed")
  case mysqlGroup info of
    Just group -> withCString group $ \group_ -> do
                      _ <- mysql_options mysql_ 5 (castPtr group_)
{-# LINE 112 "Database/HDBC/MySQL/Connection.hsc" #-}
                      return ()
    Nothing -> return ()
  withCString (mysqlHost info) $ \host_ ->
      withCString (mysqlUser info) $ \user_ ->
          withCString (mysqlPassword info) $ \passwd_ ->
              withCString (mysqlDatabase info) $ \db_ ->
                  withCString (mysqlUnixSocket info) $ \unixSocket_ ->
                      do rv <- mysql_real_connect mysql_ host_ user_ passwd_ db_
                                                  (fromIntegral $ mysqlPort info)
                                                  unixSocket_
                         when (rv == nullPtr) (connectionError mysql_)
                         wrap mysql_
    where
      -- Returns the HDBC wrapper for the native MySQL connection
      -- object.
      wrap :: Ptr MYSQL -> IO Connection
      wrap mysql_ = do
        clientver <- peekCString =<< mysql_get_client_info
        serverver <- peekCString =<< mysql_get_server_info mysql_
        protover <- mysql_get_proto_info mysql_

        -- HDBC assumes that there is no such thing as auto-commit.
        -- So we'll turn it off here and start our first transaction.
        _ <- mysql_autocommit mysql_ 0

        mysql__ <- newForeignPtr mysql_close mysql_
        doStartTransaction mysql__

        return $ Connection
                   { disconnect           = finalizeForeignPtr mysql__
                   , commit               = doCommit mysql__ >> doStartTransaction mysql__
                   , rollback             = doRollback mysql__ >> doStartTransaction mysql__
                   , run                  = doRun mysql__
                   , runRaw               = flip doQuery mysql__
                   , prepare              = newStatement mysql__
                   , clone                = connectMySQL info
                   , hdbcDriverName       = "mysql"
                   , hdbcClientVer        = clientver
                   , proxiedClientName    = "mysql"
                   , proxiedClientVer     = show protover
                   , dbServerVer          = serverver
                   , dbTransactionSupport = True
                   , getTables            = doGetTables mysql__
                   , describeTable        = doDescribeTable mysql__
                   }

-- A MySQL statement: wraps mysql.h's MYSQL_STMT struct.
data MYSQL_STMT

-- A MySQL result: wraps mysql.h's MYSQL_RES struct.
data MYSQL_RES

-- A MySQL field: wraps mysql.h's MYSQL_FIELD struct.  We do actually
-- have to spelunk this structure, so it's a Storable instance.
data MYSQL_FIELD = MYSQL_FIELD
    { fieldName      :: String
    , fieldLength    :: CULong
    , fieldMaxLength :: CULong
    , fieldType      :: CInt
    , fieldDecimals  :: CUInt
    , fieldFlags     :: CUInt
    }

instance Storable MYSQL_FIELD where
    sizeOf _     = 128
{-# LINE 177 "Database/HDBC/MySQL/Connection.hsc" #-}
    alignment _  = alignment (undefined :: CInt)

    peek p = do
      fname   <- peekCString =<< ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 181 "Database/HDBC/MySQL/Connection.hsc" #-}
      flength <- ((\hsc_ptr -> peekByteOff hsc_ptr 56)) p
{-# LINE 182 "Database/HDBC/MySQL/Connection.hsc" #-}
      fmaxlen <- ((\hsc_ptr -> peekByteOff hsc_ptr 64)) p
{-# LINE 183 "Database/HDBC/MySQL/Connection.hsc" #-}
      ftype   <- ((\hsc_ptr -> peekByteOff hsc_ptr 112)) p
{-# LINE 184 "Database/HDBC/MySQL/Connection.hsc" #-}
      fdec    <- ((\hsc_ptr -> peekByteOff hsc_ptr 104)) p
{-# LINE 185 "Database/HDBC/MySQL/Connection.hsc" #-}
      fflags  <- ((\hsc_ptr -> peekByteOff hsc_ptr 100)) p
{-# LINE 186 "Database/HDBC/MySQL/Connection.hsc" #-}
      return $ MYSQL_FIELD
                 { fieldName      = fname
                 , fieldLength    = flength
                 , fieldMaxLength = fmaxlen
                 , fieldType      = ftype
                 , fieldDecimals  = fdec
                 , fieldFlags     = fflags
                 }

    poke _ _ = error "MYSQL_FIELD: poke"

-- A MySQL binding to a query parameter or result.  This wraps
-- mysql.h's MYSQL_BIND struct, and it's also Storable -- in this
-- case, so that we can create them.
data MYSQL_BIND = MYSQL_BIND
    { bindLength       :: Ptr CULong
    , bindIsNull       :: Ptr CChar
    , bindBuffer       :: Ptr ()
    , bindError        :: Ptr CChar
    , bindBufferType   :: CInt
    , bindBufferLength :: CULong
    , bindIsUnsigned   :: CChar
    }

data Signedness = Signed | Unsigned deriving (Eq)

instance Storable MYSQL_BIND where
    sizeOf _      = 112
{-# LINE 214 "Database/HDBC/MySQL/Connection.hsc" #-}
    alignment _   = alignment (undefined :: CInt)

    peek _ = error "MYSQL_BIND: peek"

    poke p (MYSQL_BIND len_ isNull_ buf_ err_ buftyp buflen unsigned) = do
        memset (castPtr p) 0 112
{-# LINE 220 "Database/HDBC/MySQL/Connection.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 0))        p len_
{-# LINE 221 "Database/HDBC/MySQL/Connection.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 8))       p isNull_
{-# LINE 222 "Database/HDBC/MySQL/Connection.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 16))        p buf_
{-# LINE 223 "Database/HDBC/MySQL/Connection.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 24))         p err_
{-# LINE 224 "Database/HDBC/MySQL/Connection.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 96))   p buftyp
{-# LINE 225 "Database/HDBC/MySQL/Connection.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 64)) p buflen
{-# LINE 226 "Database/HDBC/MySQL/Connection.hsc" #-}
        ((\hsc_ptr -> pokeByteOff hsc_ptr 101))   p unsigned
{-# LINE 227 "Database/HDBC/MySQL/Connection.hsc" #-}

data MYSQL_TIME = MYSQL_TIME
    { timeYear       :: CUInt
    , timeMonth      :: CUInt
    , timeDay        :: CUInt
    , timeHour       :: CUInt
    , timeMinute     :: CUInt
    , timeSecond     :: CUInt
    }

instance Storable MYSQL_TIME where
    sizeOf _      = 40
{-# LINE 239 "Database/HDBC/MySQL/Connection.hsc" #-}
    alignment _   = alignment (undefined :: CInt)

    peek p = do
      year    <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) p
{-# LINE 243 "Database/HDBC/MySQL/Connection.hsc" #-}
      month   <- ((\hsc_ptr -> peekByteOff hsc_ptr 4)) p
{-# LINE 244 "Database/HDBC/MySQL/Connection.hsc" #-}
      day     <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) p
{-# LINE 245 "Database/HDBC/MySQL/Connection.hsc" #-}
      hour    <- ((\hsc_ptr -> peekByteOff hsc_ptr 12)) p
{-# LINE 246 "Database/HDBC/MySQL/Connection.hsc" #-}
      minute  <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) p
{-# LINE 247 "Database/HDBC/MySQL/Connection.hsc" #-}
      second  <- ((\hsc_ptr -> peekByteOff hsc_ptr 20)) p
{-# LINE 248 "Database/HDBC/MySQL/Connection.hsc" #-}
      return (MYSQL_TIME year month day hour minute second)

    poke p t = do
      memset (castPtr p) 0 40
{-# LINE 252 "Database/HDBC/MySQL/Connection.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0))   p (timeYear t)
{-# LINE 253 "Database/HDBC/MySQL/Connection.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 4))  p (timeMonth t)
{-# LINE 254 "Database/HDBC/MySQL/Connection.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8))    p (timeDay t)
{-# LINE 255 "Database/HDBC/MySQL/Connection.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12))   p (timeHour t)
{-# LINE 256 "Database/HDBC/MySQL/Connection.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) p (timeMinute t)
{-# LINE 257 "Database/HDBC/MySQL/Connection.hsc" #-}
      ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) p (timeSecond t)
{-# LINE 258 "Database/HDBC/MySQL/Connection.hsc" #-}

-- Prepares a new Statement for execution.
newStatement :: ForeignPtr MYSQL -> String -> IO Types.Statement
newStatement mysql__ query = withForeignPtr mysql__ $ \mysql_ -> do
  stmt_ <- mysql_stmt_init mysql_
  when (stmt_ == nullPtr) (connectionError mysql_)

  -- If an error occurs below, we'll lose the reference to the foreign
  -- pointer and run the finalizer.
  stmt__ <- Foreign.Concurrent.newForeignPtr stmt_ (mysql_stmt_close stmt_)

  withCStringLen query $ \(query_, len) -> do
      rv <- mysql_stmt_prepare stmt_ query_ (fromIntegral len)
      when (rv /= 0) (statementError stmt_)

  -- Collect the result fields of the statement; this will simply be
  -- the empty list if we're doing something that doesn't generate
  -- results.
  fields <- fieldsOf stmt_

  -- Create MYSQL_BIND structures for each field and point the the
  -- statement at those buffers.  Again, if there are no fields,
  -- this'll be a no-op.
  results <- mapM resultOfField fields
  when (not $ null results)
       (withArray results $ \bind_ -> do
          rv' <- mysql_stmt_bind_result stmt_ bind_
          when (rv' /= 0) (statementError stmt_))

  Foreign.Concurrent.addForeignPtrFinalizer stmt__ (freeBinds results)

  -- We pass the connection ForeignPtr down to execute and fetchRow as
  -- a silly way to keep a reference to it alive so long as the
  -- statement is around.
  return $ Types.Statement
             { Types.execute        = execute mysql__ stmt__
             , Types.executeRaw     = executeRaw mysql__ stmt__
             , Types.executeMany    = mapM_ $ execute mysql__ stmt__
             , Types.finish         = finalizeForeignPtr stmt__
             , Types.fetchRow       = fetchRow mysql__ stmt__ results
             , Types.originalQuery  = query
             , Types.getColumnNames = return $ map fieldName fields
             , Types.describeResult = return $ map sqlColDescOf fields
             }

-- Release the storage allocated for each bind.
freeBinds :: [MYSQL_BIND] -> IO ()
freeBinds binds = do
  mapM_ freeOneBind binds
    where freeOneBind bind = do
            free $ bindLength bind
            free $ bindIsNull bind
            free $ bindBuffer bind
            free $ bindError bind

-- Returns the list of fields from a prepared statement.
fieldsOf :: Ptr MYSQL_STMT -> IO [MYSQL_FIELD]
fieldsOf stmt_ = bracket acquire release fieldsOf'
    where acquire                          = mysql_stmt_result_metadata stmt_
          release res_ | res_ == nullPtr   = return ()
                       | otherwise         = mysql_free_result res_
          fieldsOf' res_ | res_ == nullPtr = return []
                         | otherwise       = fieldsOfResult res_

-- Builds the list of fields from the result set metadata: this is
-- just a helper function for fieldOf, above.
fieldsOfResult :: Ptr MYSQL_RES -> IO [MYSQL_FIELD]
fieldsOfResult res_ = do
  field_ <- mysql_fetch_field res_
  if (field_ == nullPtr)
    then return []
    else liftM2 (:) (peek field_) (fieldsOfResult res_)

-- Executes a statement with the specified binding parameters.
execute :: ForeignPtr MYSQL -> ForeignPtr MYSQL_STMT -> [Types.SqlValue] -> IO Integer
execute mysql__ stmt__ params =
    withForeignPtr mysql__ $ \_ ->
        withForeignPtr stmt__ $ \stmt_ -> do
          binds <- bindParams stmt_ params
          rv <- mysql_stmt_execute stmt_
          freeBinds binds
          when (rv /= 0) (statementError stmt_)
          nrows <- mysql_stmt_affected_rows stmt_

          -- mysql_stmt_affected_rows returns -1 when called on a
          -- SELECT statement; the HDBC API expects zero to be
          -- returned in this case.
          return $ fromIntegral (if nrows == (-1 :: CULLong) then 0 else nrows)

-- Binds placeholder parameters to values.
bindParams :: Ptr MYSQL_STMT -> [Types.SqlValue] -> IO [MYSQL_BIND]
bindParams stmt_ params = do
  param_count <- mysql_stmt_param_count stmt_
  let nparams = fromIntegral param_count

  -- XXX i'm not sure if it makes more sense to keep this paranoia, or
  -- to simply remove it.  The code that immediately follows pads
  -- extra bind parameters with nulls.
  when (nparams /= length params)
           (error "the number of parameter placeholders in the prepared SQL is different than the number of parameters provided")

  let params' = take nparams $ params ++ repeat Types.SqlNull
  binds <- mapM bindOfSqlValue params'
  withArray binds $ \bind_ -> do
      rv <- mysql_stmt_bind_param stmt_ bind_
      when (rv /= 0) (statementError stmt_)

  return binds

-- Given a SqlValue, return a MYSQL_BIND structure that we can use to
-- pass its value.
bindOfSqlValue :: Types.SqlValue -> IO MYSQL_BIND

bindOfSqlValue Types.SqlNull = do
  isNull_ <- new (1 :: CChar)
  return $ MYSQL_BIND
             { bindLength       = nullPtr
             , bindIsNull       = isNull_
             , bindBuffer       = nullPtr
             , bindError        = nullPtr
             , bindBufferType   = 6
{-# LINE 379 "Database/HDBC/MySQL/Connection.hsc" #-}
             , bindBufferLength = 0
             , bindIsUnsigned   = 0
             }

bindOfSqlValue (Types.SqlString s) = do
  bindOfSqlValue (Types.SqlByteString $ fromString s)

bindOfSqlValue (Types.SqlByteString s) = do
  B.useAsCString s $ \c_ -> do
    let len = B.length s
    buf_ <- mallocBytes len
    copyBytes buf_ c_ len
    bindOfSqlValue' len buf_ 253 Signed
{-# LINE 392 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlInteger n) = do
  buf_ <- new (fromIntegral n :: CLLong)
  bindOfSqlValue' (8::Int) buf_ 8 Signed
{-# LINE 396 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlBool b) = do
  buf_ <- new (if b then 1 else 0 :: CChar)
  bindOfSqlValue' (1::Int) buf_ 1 Signed
{-# LINE 400 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlChar c) = do
  buf_ <- new c
  bindOfSqlValue' (1::Int) buf_ 1 Signed
{-# LINE 404 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlDouble d) = do
  buf_ <- new (realToFrac d :: CDouble)
  bindOfSqlValue' (8::Int) buf_ 5 Signed
{-# LINE 408 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlInt32 n) = do
  buf_ <- new n
  bindOfSqlValue' (4::Int) buf_ 3 Signed
{-# LINE 412 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlInt64 n) = do
  buf_ <- new n
  bindOfSqlValue' (8::Int) buf_ 8 Signed
{-# LINE 416 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlRational n) = do
  buf_ <- new (realToFrac n :: CDouble)
  bindOfSqlValue' (8::Int) buf_ 5 Signed
{-# LINE 420 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlWord32 n) = do
  buf_ <- new n
  bindOfSqlValue' (4::Int) buf_ 3 Unsigned
{-# LINE 424 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlWord64 n) = do
  buf_ <- new n
  bindOfSqlValue' (8::Int) buf_ 8 Unsigned
{-# LINE 428 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlEpochTime epoch) = 
  bindOfSqlValue (Types.SqlUTCTime t)
    where t = posixSecondsToUTCTime (fromIntegral epoch)
                                            
bindOfSqlValue (Types.SqlUTCTime utct) = do
  let t = utcToMysqlTime utct
  buf_ <- new t
  bindOfSqlValue' (40::Int) buf_ 12 Signed
{-# LINE 437 "Database/HDBC/MySQL/Connection.hsc" #-}
      where utcToMysqlTime :: UTCTime -> MYSQL_TIME
            utcToMysqlTime (UTCTime day difftime) =
                let (y, m, d) = toGregorian day
                    t  = floor $ (realToFrac difftime :: Double)
                    h  = t `div` 3600
                    mn = t `div` 60 `mod` 60
                    s  = t `mod` 60
                in MYSQL_TIME (fromIntegral y) (fromIntegral m) (fromIntegral d) h mn s

bindOfSqlValue (Types.SqlTimeDiff n) = do
  let h  = fromIntegral $ n `div` 3600
      mn = fromIntegral $ n `div` 60 `mod` 60
      s  = fromIntegral $ n `mod` 60
      t  = MYSQL_TIME 0 0 0 h mn s
  buf_ <- new t
  bindOfSqlValue' (40::Int) buf_ 11 Signed
{-# LINE 453 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlLocalDate day) = do
  let (y, m, d) = toGregorian day
      t         = MYSQL_TIME (fromIntegral y) (fromIntegral m) (fromIntegral d) 0 0 0
  buf_ <- new t
  bindOfSqlValue' (40::Int) buf_ 10 Signed
{-# LINE 459 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlLocalTimeOfDay time) = do
  let h  = fromIntegral $ todHour time
      mn = fromIntegral $ todMin time
      s  = floor $ todSec time
      t  = MYSQL_TIME 0 0 0 h mn s
  buf_ <- new t
  bindOfSqlValue' (40::Int) buf_ 11 Signed
{-# LINE 467 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlZonedLocalTimeOfDay t _) =
  bindOfSqlValue $ Types.SqlLocalTimeOfDay t

bindOfSqlValue (Types.SqlLocalTime (LocalTime day time)) = do
  let (y, m, d) = toGregorian day
      h         = fromIntegral $ todHour time
      mn        = fromIntegral $ todMin time
      s         = floor $ todSec time
      t         = MYSQL_TIME (fromIntegral y) (fromIntegral m) (fromIntegral d) h mn s
  buf_ <- new t
  bindOfSqlValue' (40::Int) buf_ 12 Signed
{-# LINE 479 "Database/HDBC/MySQL/Connection.hsc" #-}

bindOfSqlValue (Types.SqlZonedTime t) =
  bindOfSqlValue $ Types.SqlLocalTime $ zonedTimeToLocalTime t

bindOfSqlValue (Types.SqlDiffTime t) =
  bindOfSqlValue $ Types.SqlPOSIXTime t

bindOfSqlValue (Types.SqlPOSIXTime t) =
  bindOfSqlValue $ Types.SqlUTCTime $ posixSecondsToUTCTime t

-- A nasty helper function that cuts down on the boilerplate a bit.
bindOfSqlValue' :: (Integral a, Storable b) => a -> Ptr b -> CInt -> Signedness -> IO MYSQL_BIND

bindOfSqlValue' len buf_ btype signedness = do
  let buflen = fromIntegral len
  isNull_ <- new (0 :: CChar)
  len_ <- new buflen
  return $ MYSQL_BIND
             { bindLength       = len_
             , bindIsNull       = isNull_
             , bindBuffer       = castPtr buf_
             , bindError        = nullPtr
             , bindBufferType   = btype
             , bindBufferLength = buflen
             , bindIsUnsigned   = (if signedness == Unsigned then 1 else 0)
             }

executeRaw :: ForeignPtr MYSQL -> ForeignPtr MYSQL_STMT -> IO ()
executeRaw mysql__ stmt__ =
    withForeignPtr mysql__ $ \_ ->
        withForeignPtr stmt__ $ \stmt_ -> do
          rv <- mysql_stmt_execute stmt_
          when (rv /= 0) (statementError stmt_)

-- Returns an appropriate binding structure for a field.
resultOfField :: MYSQL_FIELD -> IO MYSQL_BIND
resultOfField field =
    let ftype    = fieldType field
        unsigned = (fieldFlags field .&. 32) /= 0
{-# LINE 518 "Database/HDBC/MySQL/Connection.hsc" #-}
        btype    = boundType ftype (fieldDecimals field)
        size     = boundSize btype (fieldLength field) in
    do size_   <- new size
       isNull_ <- new (0 :: CChar)
       error_  <- new (0 :: CChar)
       buffer_ <- mallocBytes (fromIntegral size)
       return $ MYSQL_BIND { bindLength       = size_
                           , bindIsNull       = isNull_
                           , bindBuffer       = buffer_
                           , bindError        = error_
                           , bindBufferType   = btype
                           , bindBufferLength = size
                           , bindIsUnsigned   = if unsigned then 1 else 0
                           }

-- Returns the appropriate result type for a particular host type.
boundType :: CInt -> CUInt -> CInt
boundType 254     _ = 253
{-# LINE 536 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 1       _ = 3
{-# LINE 537 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 2      _ = 3
{-# LINE 538 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 9      _ = 3
{-# LINE 539 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 13       _ = 3
{-# LINE 540 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 247       _ = 3
{-# LINE 541 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 0    0 = 8
{-# LINE 542 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 0    _ = 5
{-# LINE 543 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 246 0 = 8
{-# LINE 544 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 246 _ = 5
{-# LINE 545 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 4      _ = 5
{-# LINE 546 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType 252       _ = 253
{-# LINE 547 "Database/HDBC/MySQL/Connection.hsc" #-}
boundType t                              _ = t

-- Returns the amount of storage required for a particular result
-- type.
boundSize :: CInt -> CULong -> CULong
boundSize 3      _ = 4
{-# LINE 553 "Database/HDBC/MySQL/Connection.hsc" #-}
boundSize 5    _ = 8
{-# LINE 554 "Database/HDBC/MySQL/Connection.hsc" #-}
boundSize 12  _ = 40
{-# LINE 555 "Database/HDBC/MySQL/Connection.hsc" #-}
boundSize 11      _ = 40
{-# LINE 556 "Database/HDBC/MySQL/Connection.hsc" #-}
boundSize 14   _ = 40
{-# LINE 557 "Database/HDBC/MySQL/Connection.hsc" #-}
boundSize 10      _ = 40
{-# LINE 558 "Database/HDBC/MySQL/Connection.hsc" #-}
boundSize 7 _ = 40
{-# LINE 559 "Database/HDBC/MySQL/Connection.hsc" #-}
boundSize _                             n = n

-- Fetches a row from an executed statement and converts the cell
-- values into a list of SqlValue types.
fetchRow :: ForeignPtr MYSQL -> ForeignPtr MYSQL_STMT -> [MYSQL_BIND] -> IO (Maybe [Types.SqlValue])
fetchRow mysql__ stmt__ results =
  withForeignPtr mysql__ $ \_ ->
      withForeignPtr stmt__ $ \stmt_ -> do
          rv <- mysql_stmt_fetch stmt_
          case rv of
            0                             -> row
            101 -> liftM Just $ mapM (uncurry $ fill stmt_) $ zip [0..] results
{-# LINE 571 "Database/HDBC/MySQL/Connection.hsc" #-}
            100        -> finalizeForeignPtr stmt__ >> return Nothing
{-# LINE 572 "Database/HDBC/MySQL/Connection.hsc" #-}
            _                             -> statementError stmt_
    where row = liftM Just $ mapM cellValue results
          fill stmt_ column bind = do
            err <- peek $ bindError bind
            if err == 1 then do len <- peek $ bindLength bind
                                bracket (mallocBytes $ fromIntegral len) free $ \buffer_ ->
                                    do let tempBind = bind { bindBuffer = buffer_, bindBufferLength = len }
                                       rv <- with tempBind $ \bind_ -> mysql_stmt_fetch_column stmt_ bind_ column 0
                                       when (rv /= 0) (statementError stmt_)
                                       cellValue tempBind
                        else cellValue bind

-- Produces a single SqlValue cell value given the binding, handling
-- nulls appropriately.
cellValue :: MYSQL_BIND -> IO Types.SqlValue
cellValue bind = do
  isNull <- peek $ bindIsNull bind
  if isNull == 0 then cellValue' else return Types.SqlNull
      where cellValue' = do
                   len <- peek $ bindLength bind
                   let buftype  = bindBufferType bind
                       buf      = bindBuffer bind
                       unsigned = bindIsUnsigned bind == 1
                   nonNullCellValue buftype buf len unsigned

-- Produces a single SqlValue from the binding's type and buffer
-- pointer.  It assumes that the value is not null.
nonNullCellValue :: CInt -> Ptr () -> CULong -> Bool -> IO Types.SqlValue

nonNullCellValue 3 p _ u = do
{-# LINE 602 "Database/HDBC/MySQL/Connection.hsc" #-}
  n :: CInt <- peek $ castPtr p
  return $ if u then Types.SqlWord32 (fromIntegral n)
                else Types.SqlInt32 (fromIntegral n)

nonNullCellValue 8 p _ u = do
{-# LINE 607 "Database/HDBC/MySQL/Connection.hsc" #-}
  n :: CLLong <- peek $ castPtr p
  return $ if u then Types.SqlWord64 (fromIntegral n)
                else Types.SqlInt64 (fromIntegral n)

nonNullCellValue 5 p _ _ = do
{-# LINE 612 "Database/HDBC/MySQL/Connection.hsc" #-}
  n :: CDouble <- peek $ castPtr p
  return $ Types.SqlDouble (realToFrac n)

nonNullCellValue 253 p len _ =
{-# LINE 616 "Database/HDBC/MySQL/Connection.hsc" #-}
    B.packCStringLen ((castPtr p), fromIntegral len) >>= return . Types.SqlByteString

nonNullCellValue 7 p _ _ = do
{-# LINE 619 "Database/HDBC/MySQL/Connection.hsc" #-}
  t :: MYSQL_TIME <- peek $ castPtr p
  let secs = (utcTimeToPOSIXSeconds . mysqlTimeToUTC) t
  return $ Types.SqlPOSIXTime secs
      where mysqlTimeToUTC :: MYSQL_TIME -> UTCTime
            mysqlTimeToUTC (MYSQL_TIME y m d h mn s) =
                -- XXX so, this is fine if the date we're getting back
                -- is UTC.  If not, well, it's wrong.
                let day = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
                    time = s + mn * 60 + h * 3600
                in UTCTime day (secondsToDiffTime $ fromIntegral time)

nonNullCellValue 12 p _ _ = do
{-# LINE 631 "Database/HDBC/MySQL/Connection.hsc" #-}
  (MYSQL_TIME y m d h mn s) <- peek $ castPtr p
  let date = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
      time = TimeOfDay (fromIntegral h) (fromIntegral mn) (fromIntegral s)
  return $ Types.SqlLocalTime (LocalTime date time)

nonNullCellValue 11 p _ _ = do
{-# LINE 637 "Database/HDBC/MySQL/Connection.hsc" #-}
  (MYSQL_TIME _ _ _ h mn s) <- peek $ castPtr p
  let time = TimeOfDay (fromIntegral h) (fromIntegral mn) (fromIntegral s)
  return $ Types.SqlLocalTimeOfDay time

nonNullCellValue 10 p _ _ = do
{-# LINE 642 "Database/HDBC/MySQL/Connection.hsc" #-}
  (MYSQL_TIME y m d _ _ _) <- peek $ castPtr p
  let date = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
  return $ Types.SqlLocalDate date

nonNullCellValue 14 p _ _ = do
{-# LINE 647 "Database/HDBC/MySQL/Connection.hsc" #-}
  (MYSQL_TIME y m d _ _ _) <- peek $ castPtr p
  let date = fromGregorian (fromIntegral y) (fromIntegral m) (fromIntegral d)
  return $ Types.SqlLocalDate date

nonNullCellValue t _ _ _ = return $ Types.SqlString ("unknown type " ++ show t)

-- Cough up the column metadata for a field that's returned from a
-- query.
sqlColDescOf :: MYSQL_FIELD -> (String, ColTypes.SqlColDesc)
sqlColDescOf f =
    let typ      = typeIdOf (fieldType f)
        sz       = Just $ fromIntegral $ fieldLength f
        octlen   = Just $ fromIntegral $ fieldLength f
        digits   = Just $ fromIntegral $ fieldDecimals f
        nullable = Just $ (fieldFlags f .&. 1) == 0
{-# LINE 662 "Database/HDBC/MySQL/Connection.hsc" #-}
    in (fieldName f, ColTypes.SqlColDesc typ sz octlen digits nullable)

-- Returns the HDBC column type appropriate for the MySQL column
-- type. (XXX as far as I can tell, I can't tell the difference
-- between a TEXT and a BLOB column, here.)
typeIdOf :: CInt -> ColTypes.SqlTypeId
typeIdOf 0     = ColTypes.SqlDecimalT
{-# LINE 669 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 1        = ColTypes.SqlTinyIntT
{-# LINE 670 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 2       = ColTypes.SqlSmallIntT
{-# LINE 671 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 3        = ColTypes.SqlIntegerT
{-# LINE 672 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 4       = ColTypes.SqlFloatT
{-# LINE 673 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 5      = ColTypes.SqlDoubleT
{-# LINE 674 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 6        = ColTypes.SqlUnknownT "NULL"
{-# LINE 675 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 7   = ColTypes.SqlTimestampT
{-# LINE 676 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 8    = ColTypes.SqlNumericT
{-# LINE 677 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 9       = ColTypes.SqlIntegerT
{-# LINE 678 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 10        = ColTypes.SqlDateT
{-# LINE 679 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 11        = ColTypes.SqlTimeT
{-# LINE 680 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 12    = ColTypes.SqlTimestampT
{-# LINE 681 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 13        = ColTypes.SqlNumericT
{-# LINE 682 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 14     = ColTypes.SqlDateT
{-# LINE 683 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 15     = ColTypes.SqlVarCharT
{-# LINE 684 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 16         = ColTypes.SqlBitT
{-# LINE 685 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 246  = ColTypes.SqlDecimalT
{-# LINE 686 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 247        = ColTypes.SqlUnknownT "ENUM"
{-# LINE 687 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 248         = ColTypes.SqlUnknownT "SET"
{-# LINE 688 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 249   = ColTypes.SqlBinaryT
{-# LINE 689 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 250 = ColTypes.SqlBinaryT
{-# LINE 690 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 251   = ColTypes.SqlBinaryT
{-# LINE 691 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 252        = ColTypes.SqlBinaryT
{-# LINE 692 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 253  = ColTypes.SqlVarCharT
{-# LINE 693 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 254      = ColTypes.SqlCharT
{-# LINE 694 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf 255    = ColTypes.SqlUnknownT "GEOMETRY"
{-# LINE 695 "Database/HDBC/MySQL/Connection.hsc" #-}
typeIdOf n                               = ColTypes.SqlUnknownT ("unknown type " ++ show n)

-- Run a query and discard the results, if any.
doRun :: ForeignPtr MYSQL -> String -> [Types.SqlValue] -> IO Integer
doRun mysql__ query params = do
  stmt <- newStatement mysql__ query
  rv <- Types.execute stmt params
  Types.finish stmt
  return rv

-- Issue a query "old school", without using the prepared statement
-- API.  We use this internally to send the transaction-related
-- statements, because -- it turns out -- we have to!
doQuery :: String -> ForeignPtr MYSQL -> IO ()
doQuery stmt mysql__ = withForeignPtr mysql__ $ \mysql_ -> do
    withCString stmt $ \stmt_ -> do
      rv <- mysql_query mysql_ stmt_
      when (rv /= 0) (connectionError mysql_)

doCommit :: ForeignPtr MYSQL -> IO ()
doCommit = doQuery "COMMIT"

doRollback :: ForeignPtr MYSQL -> IO ()
doRollback = doQuery "ROLLBACK"

doStartTransaction :: ForeignPtr MYSQL -> IO ()
doStartTransaction = doQuery "START TRANSACTION"

-- Retrieve all the tables in the current database by issuing a "SHOW
-- TABLES" statement.
doGetTables :: ForeignPtr MYSQL -> IO [String]
doGetTables mysql__ = do
  stmt <- newStatement mysql__ "SHOW TABLES"
  _ <- Types.execute stmt []
  rows <- unfoldRows stmt
  Types.finish stmt
  return $ map (fromSql . head) rows
      where fromSql :: Types.SqlValue -> String
            fromSql (Types.SqlByteString s) = toString s
            fromSql _                       = error "SHOW TABLES returned a table whose name wasn't a string"

-- Describe a single table in the database by issuing a "DESCRIBE"
-- statement and parsing the results.  (XXX this is sloppy right now;
-- ideally you'd come up with exactly the same results as if you did a
-- describeResult on SELECT * FROM table.)
doDescribeTable :: ForeignPtr MYSQL -> String -> IO [(String, ColTypes.SqlColDesc)]
doDescribeTable mysql__ table = do
  stmt <- newStatement mysql__ ("DESCRIBE " ++ table)
  _ <- Types.execute stmt []
  rows <- unfoldRows stmt
  Types.finish stmt
  return $ map fromRow rows
      where fromRow :: [Types.SqlValue] -> (String, ColTypes.SqlColDesc)
            fromRow ((Types.SqlByteString colname)
                     :(Types.SqlByteString coltype)
                     :(Types.SqlByteString nullAllowed):_) =
                let sqlTypeId = typeIdOfString $ toString coltype
                    -- XXX parse the column width and decimals, too!
                    nullable = Just $ toString nullAllowed == "YES"
                in (toString colname, ColTypes.SqlColDesc sqlTypeId Nothing Nothing Nothing nullable)

            fromRow _ = error "DESCRIBE failed"

-- XXX this is likely to be incomplete.
typeIdOfString :: String -> ColTypes.SqlTypeId
typeIdOfString s
    | "int"       `isPrefixOf` s = ColTypes.SqlIntegerT
    | "bigint"    `isPrefixOf` s = ColTypes.SqlIntegerT
    | "smallint"  `isPrefixOf` s = ColTypes.SqlSmallIntT
    | "mediumint" `isPrefixOf` s = ColTypes.SqlIntegerT
    | "tinyint"   `isPrefixOf` s = ColTypes.SqlTinyIntT
    | "decimal"   `isPrefixOf` s = ColTypes.SqlDecimalT
    | "double"    `isPrefixOf` s = ColTypes.SqlDoubleT
    | "float"     `isPrefixOf` s = ColTypes.SqlFloatT
    | "char"      `isPrefixOf` s = ColTypes.SqlCharT
    | "varchar"   `isPrefixOf` s = ColTypes.SqlVarCharT
    | "text"      `isPrefixOf` s = ColTypes.SqlBinaryT
    | "timestamp" `isPrefixOf` s = ColTypes.SqlTimestampT
    | "datetime"  `isPrefixOf` s = ColTypes.SqlTimestampT
    | "date"      `isPrefixOf` s = ColTypes.SqlDateT
    | "time"      `isPrefixOf` s = ColTypes.SqlTimeT
    | otherwise                  = ColTypes.SqlUnknownT s

-- A helper function that turns an executed statement into the
-- resulting rows.
unfoldRows :: Types.Statement -> IO [[Types.SqlValue]]
unfoldRows stmt = do
  row <- Types.fetchRow stmt
  case row of
    Nothing     -> return []
    Just (vals) -> do rows <- unfoldRows stmt
                      return (vals : rows)

-- Returns the last statement-level error.
statementError :: Ptr MYSQL_STMT -> IO a
statementError stmt_ = do
  errno <- mysql_stmt_errno stmt_
  msg <- peekCString =<< mysql_stmt_error stmt_
  throwSqlError $ Types.SqlError "" (fromIntegral errno) msg

-- Returns the last connection-level error.
connectionError :: Ptr MYSQL -> IO a
connectionError mysql_ = do
  errno <- mysql_errno mysql_
  msg <- peekCString =<< mysql_error mysql_
  throwSqlError $ Types.SqlError "" (fromIntegral errno) msg

{- ---------------------------------------------------------------------- -}

-- Here are all the FFI imports.

foreign import ccall unsafe mysql_get_client_info
    :: IO CString

foreign import ccall unsafe mysql_get_server_info
    :: Ptr MYSQL -> IO CString

foreign import ccall unsafe mysql_get_proto_info
    :: Ptr MYSQL -> IO CUInt

foreign import ccall unsafe mysql_init
 :: Ptr MYSQL
 -> IO (Ptr MYSQL)

foreign import ccall unsafe mysql_options
 :: Ptr MYSQL
 -> CInt
 -> Ptr ()
 -> IO CInt

foreign import ccall unsafe mysql_real_connect
 :: Ptr MYSQL -- the context
 -> CString   -- hostname
 -> CString   -- username
 -> CString   -- password
 -> CString   -- database
 -> CInt      -- port
 -> CString   -- unix socket
 -> IO (Ptr MYSQL)

foreign import ccall unsafe "&mysql_close" mysql_close
    :: FunPtr (Ptr MYSQL -> IO ())

foreign import ccall unsafe mysql_stmt_init
    :: Ptr MYSQL -> IO (Ptr MYSQL_STMT)

foreign import ccall unsafe mysql_stmt_prepare
    :: Ptr MYSQL_STMT -> CString -> CInt -> IO CInt

foreign import ccall unsafe mysql_stmt_result_metadata
    :: Ptr MYSQL_STMT -> IO (Ptr MYSQL_RES)

foreign import ccall unsafe mysql_stmt_bind_param
    :: Ptr MYSQL_STMT -> Ptr MYSQL_BIND -> IO CChar

foreign import ccall unsafe mysql_stmt_bind_result
    :: Ptr MYSQL_STMT -> Ptr MYSQL_BIND -> IO CChar

foreign import ccall unsafe mysql_stmt_param_count
    :: Ptr MYSQL_STMT -> IO CULong

foreign import ccall unsafe mysql_free_result
    :: Ptr MYSQL_RES -> IO ()

foreign import ccall unsafe mysql_stmt_execute
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall unsafe mysql_stmt_affected_rows
    :: Ptr MYSQL_STMT -> IO CULLong

foreign import ccall unsafe mysql_fetch_field
    :: Ptr MYSQL_RES -> IO (Ptr MYSQL_FIELD)

foreign import ccall unsafe mysql_stmt_fetch
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall unsafe mysql_stmt_fetch_column
    :: Ptr MYSQL_STMT -> Ptr MYSQL_BIND -> CUInt -> CULong -> IO CInt

foreign import ccall unsafe mysql_stmt_close
    :: Ptr MYSQL_STMT -> IO ()

foreign import ccall unsafe mysql_stmt_errno
    :: Ptr MYSQL_STMT -> IO CInt

foreign import ccall unsafe mysql_stmt_error
    :: Ptr MYSQL_STMT -> IO CString

foreign import ccall unsafe mysql_errno
    :: Ptr MYSQL -> IO CInt

foreign import ccall unsafe mysql_error
    :: Ptr MYSQL -> IO CString

foreign import ccall unsafe mysql_autocommit
    :: Ptr MYSQL -> CChar -> IO CChar

foreign import ccall unsafe mysql_query
    :: Ptr MYSQL -> CString -> IO CInt

foreign import ccall unsafe memset
    :: Ptr () -> CInt -> CSize -> IO ()

