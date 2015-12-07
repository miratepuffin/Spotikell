module MySqlConnect (getConnection, closeConnection) where
import Database.HDBC
import Database.HDBC.MySQL

mySQLInfo :: MySQLConnectInfo
mySQLInfo = defaultMySQLConnectInfo {
  mysqlHost = "localhost",
  mysqlDatabase = "spotify",
  mysqlUser = "root",
  mysqlPassword = "1234",
  mysqlUnixSocket = "/var/run/mysqld/mysqld.sock" 
}
getConnection :: IO Connection
getConnection = do 
  conn <- connectMySQL mySQLInfo
  return conn

closeConnection :: Connection -> IO ()
closeConnection conn = do
  commit conn
  disconnect conn