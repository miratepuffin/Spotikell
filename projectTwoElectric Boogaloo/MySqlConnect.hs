module MySqlConnect (getConnection, closeConnection) where
import Database.HDBC
import Database.HDBC.MySQL
{- There are two sets of mySQLInfo as it is possible to work with a local database and one online
   Interestingly, Amazon Web Service has an issue connecting its vertual servers to its databases
   So  I have to use a local server on that machine. This is also a lot faster as the AWS databases
   Are very slow. Still the information is there if required, such as running the server on a windows machine-}
mySQLInfo :: MySQLConnectInfo
mySQLInfo = defaultMySQLConnectInfo {
  mysqlHost = "localhost",
  mysqlDatabase = "spotify",
  mysqlUser = "root",
  mysqlPassword = "1234",
  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" 
}

--remote database on amazon
mySQLInfo' :: MySQLConnectInfo
mySQLInfo' = defaultMySQLConnectInfo {
  mysqlHost = "spotikell.cp3m1q1ydb0h.eu-west-1.rds.amazonaws.com",
  mysqlDatabase = "spotify",
  mysqlUser = "root",
  mysqlPassword = "12345678",
  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" 
}

{-| The getConnection Function returns a MySQL connection object attached to either a local database
    Or the AWS database above, this can then be used to make reqests to said database -}
getConnection :: IO Connection
getConnection = do 
  conn <- connectMySQL mySQLInfo
  return conn

{-| The close connection function takes a Connection object commits the changes made using it and closes the database connection
    These are placed here in a singular method to make sure both are always called  -}
closeConnection :: Connection -> IO ()
closeConnection conn = do
  commit conn
  disconnect conn