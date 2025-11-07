test_that("function fetches data", {
  expect_no_error({
    res <- get_community_matrix(dataset = "NorIns",
                                id_status = "Testing")

  })

  expect_true(nrow(res) > 1)

})
