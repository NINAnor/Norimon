test_that("connecting to database creates object con", {
  if(exists("con")){ DBI::dbDisconnect(con)}

  connect_to_database(username = "postgjest",
                      password = "gjestpost",
                      host = "gisdata-db.nina.no",
                      dbname = "gisdata",)

  expect_s4_class(con, "PqConnection")


})
