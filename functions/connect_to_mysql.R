require(DBI)
require(RMySQL)
# require(pool)

connect_mysql = function(db_name){
  
  # load in credentials
  source("scripts/functions/db_creds.R")
  db_creds = db_inputs()
  
  # set up connection
  con <- dbConnect(MySQL(), #odbc::odbc(), #dbDriver("MySQL"), #
                   user = db_creds$db_user, 
                   password = db_creds$db_password,
                   dbname = db_name, 
                   host = db_creds$db_host, 
                   port = db_creds$db_port,
                   .connection_string = "Driver={MySQL ODBC 8.0 ANSI Driver};")

  # return connection
  return(con)
}