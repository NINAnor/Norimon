#' connect_to_database
#'
#' Best practice is to not store your username and password in any scripts. See example for a way to script retrieval of credentials
#'
#' @param username
#' @param password
#' @param host
#' @param dbname
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' Connect to insect monitoring database while storing your credentials in a password file
#'
#' dontrun{
#'
#'   source("~/.rpgpass")
#'
#'   connect_to_database(
#'      username = username,
#'      password = password
#'   )
#'
#'   rm(list = c("username", "password"))
#' }
#'
#'
#' Connect to another database
#'
#' connect_to_database(
#'   username = "postgjest",
#'   password = "gjestpost",,
#'   host = "gisdata-db.nina.no",
#'   dbname = "gisdata"
#' )
#'
#'


connect_to_database <- function(username,
                                password,
                                host = "ninradardata01.nina.no",
                                dbname = "insect_monitoring",
                                ...){

  tmp <- DBI::dbConnect(RPostgres::Postgres(),
                        host = host,
                        dbname = dbname,
                        user = username,
                        password = password,
                        ...)
  assign("con", tmp, .GlobalEnv)

}
