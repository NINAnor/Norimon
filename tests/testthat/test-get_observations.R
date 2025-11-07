test_that("gets dataset and type specifications", {
  connect_to_insect_db()

  expect_no_error({
  res <- get_observations(dataset = "TidVar",
                          id_type = "metabarcoding",
                          id_status = "Secondary")

  })

  expect_contains(class(res), "tbl")

  expect_true(nrow(res) > 1)

})
