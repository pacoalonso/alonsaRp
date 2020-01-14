context("moda")

test_that("moda works", {
    input <- c(2,3,3,3,9,1,3,1,0,2,2,1,2,3,8,9,9,1,1,0,0,1,0,2,0,9,9,9,7,2,3,2,8,5)
    output <- c(2)
    expect_equal(moda(input), output)
})

test_that("moda returns NA if NAs present", {
    input <- c(2,3,3,3,9,1,3,1,0,2,2,1,2,3,8,9,9,1,1,0,0,1,0,2,0,9,9,9,7,2,3,2,8,NA)
    output <- c(NA)
    expect_equal(moda(input), output)
})

test_that("moda returns NA if NAs present unless told not to", {
    input <- c(2,3,3,3,9,1,3,1,0,2,2,1,2,3,8,9,9,1,1,0,0,1,0,2,0,9,9,9,7,2,3,2,8,NA)
    output <- c(2)
    expect_equal(moda(input, na.rm=TRUE), output)
})



