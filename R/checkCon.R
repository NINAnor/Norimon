#' Check for an open connection to the database.
#'
#' Of internal use
#'
#' @return An error message if a connection called con is missing
#' @export
#'
#' @examples
#' \dontrun{
#' checkCon()
#' }
#'
checkCon <- function() {
  if (!exists("con")) {
    stop("No connection!")
  } else {
    if (!inherits(con, "PqConnection")) {
      stop("\"con\" is not of class \"PqConnection\". Have you connected to the database?")
    }
    if (!DBI::dbIsValid(con)) {
      stop("No connection")
    }
  }
}
