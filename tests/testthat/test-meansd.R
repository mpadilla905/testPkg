test_that("meansd devuelve una matriz", {

    expect_true(is.matrix(
        meansd(rnorm(20, 1.6, 0.2), nombre = "estatura", digitos = 2)
    ))

    expect_true(is.matrix(
        meansd(as.raw(rnorm(10, 1.6, 0.2)), nombre = "estatura", digitos = 3)
    ))

})
