test_that("cubicar funciona", {
  expect_equal(
    cubicar(2), 8
  )
  expect_error(
    cubicar(NA)
  )
})
