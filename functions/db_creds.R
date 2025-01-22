db_inputs = function()
{
  db_user = Sys.getenv("MYSQL_USER1")
  db_password = Sys.getenv("TEMP_PWD")
  # db_name = ""
  db_host =  Sys.getenv("MYSQL_IP") # 'localhost' #
  db_port = 3306
  
  return(list(db_user = db_user, 
              db_password = db_password,
              db_host = db_host,
              db_port = db_port))
  
  
}