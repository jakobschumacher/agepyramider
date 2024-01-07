test_that("data creation works", {
  expect_true(is.numeric(create_data()$age))
})
