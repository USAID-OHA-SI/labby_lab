library(DBI)
library(RPostgres)



dsn_database = 'vlprddb'   # Specify the name of your Database
dsn_hostname = 'vl-prd-db.postgres.database.azure.com'  
dsn_port = '5432'                # Specify your port number. e.g. 98939
dsn_uid = 'mkalnoky@vl-prd-db'         # Specify your username. e.g. "admin"
dsn_pwd = 'mka@pg17May'        # Specify your password. e.g. "xxx"


connec <- DBI::dbConnect(RPostgres::Postgres(), 
                    dbname = dsn_database,
                    host = dsn_hostname, 
                    port = dsn_port,
                    user = dsn_uid, 
                    password = dsn_pwd)


df <- dbGetQuery(connec, "SELECT * FROM vlprddb.vldata.error_type")