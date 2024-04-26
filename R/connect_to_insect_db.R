#' connect_to_insect_db
#'
#' Best practice is to not store your username and password in any scripts. See example for a way to script retrieval of credentials
#'
#' @param host Hostname of database
#' @param dbname Database name
#' @param ... additional parameter to DBI:dbConnect, such as username and password, if not provided in ~/.pgpass
#'
#' @return A PostgreSQL connection to a database called "con"
#' @export
#'
#' @examples
#'
#'
#' #Connect to insect monitoring database while storing your credentials in a password file (~/.pgpass)
#'
#' \dontrun{
#'
#'
#'   connect_to_insect_db()
#'
#'
#'
#' Connect to another database
#'
#' connect_to_insect_db(
#'   username = "postgjest",
#'   password = "gjestpost",,
#'   host = "gisdata-db.nina.no",
#'   dbname = "gisdata"
#' )
#'
#' }
#'
#'


connect_to_insect_db <- function(host = "T2lippgsql04.nina.no",
                                 dbname = "insect_monitoring",
                                ...){

  tmp <- DBI::dbConnect(RPostgres::Postgres(),
                        host = host,
                        dbname = dbname,
                        ...)

  assign("con", tmp, .GlobalEnv)

}
