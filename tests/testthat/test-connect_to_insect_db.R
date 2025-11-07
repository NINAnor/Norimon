test_that("connection works", {

  expect_no_error(connect_to_insect_db())

  expect_true(exists("con"))

})
