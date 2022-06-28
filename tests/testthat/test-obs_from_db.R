test_that("Fetching coleoptera data works", {


   source("~/.rpgpass", local = globalenv())

    connect_to_database(
      username = username,
      password = password
    )

    rm(list = c("username", "password"),
       envir = globalenv())


    beetles_2022 <- obs_from_db(subset_orders = "Coleoptera",
                                 agg_level = "year_locality")


    expect_s3_class(beetles_2022, "data.frame")

    expect_true(nrow(beetles_2022) > 0)
})
