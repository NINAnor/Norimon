#' connect_to_insect_db
#'
#' Best practice is to not store your username and password in any scripts. See example for a way to script retrieval of credentials
#'
#' @param myusername Username as character. Will look for a variable called username as default.
#' @param mypassword Password as character. Will look for a variable called password as default.
#' @param host Hostname of database
#' @param dbname Database name
#' @param ...
#'
#' @return A PostgreS connection to a database called "con"
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
#'   connect_to_insect_db(
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
#' connect_to_insect_db(
#'   username = "postgjest",
#'   password = "gjestpost",,
#'   host = "gisdata-db.nina.no",
#'   dbname = "gisdata"
#' )
#'
#'


connect_to_insect_db <- function(myusername = username,
                                mypassword = password,
                                host = "ninradardata01.nina.no",
                                dbname = "insect_monitoring",
                                ...){

  tmp <- DBI::dbConnect(RPostgres::Postgres(),
                        host = host,
                        dbname = dbname,
                        user = myusername,
                        password = mypassword,
                        ...)
  assign("con", tmp, .GlobalEnv)

}
