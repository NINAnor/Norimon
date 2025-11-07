test_that("fetches data", {

  expect_no_error({

    res <- get_localities(dataset = "NorIns",
                          as_sf = TRUE)

  })

  expect_true("sf" %in% class(res))
  expect_true(nrow(res) > 1)

})
