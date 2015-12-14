module MySqlConnect (getConnection, closeConnection) where
import Database.HDBC
import Database.HDBC.MySQL

mySQLInfo :: MySQLConnectInfo
mySQLInfo = defaultMySQLConnectInfo {
  mysqlHost = "spotikell.cp3m1q1ydb0h.eu-west-1.rds.amazonaws.com",
  mysqlDatabase = "spotify",
  mysqlUser = "root",
  mysqlPassword = "12345678",
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