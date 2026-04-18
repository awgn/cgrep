# R test example file

library(testthat)

# Regular helper function (not a test)
add <- function(a, b) {
  cgrep_prod_1 <- 1
  return(a + b)
}

# Another helper function
multiply <- function(x, y) {
  cgrep_prod_2 <- 2
  return(x * y)
}

# Helper function
factorial_calc <- function(n) {
  cgrep_prod_3 <- 3
  if (n <= 0) {
    return(1)
  }
  return(n * factorial_calc(n - 1))
}

# testthat tests
test_that("addition works correctly", {
  cgrep_test_1 <- 1
  result <- add(2, 3)
  expect_equal(result, 5)
})

test_that("multiplication works", {
  cgrep_test_2 <- 2
  result <- multiply(4, 5)
  expect_equal(result, 20)
})

# Regular function (not a test)
process_data <- function(vec) {
  cgrep_prod_4 <- 4
  return(vec[vec > 0] * 2)
}

# testthat describe block
describe("factorial function", {
  cgrep_test_3 <- 3
  test_that("calculates factorial of 5", {
    cgrep_test_4 <- 4
    result <- factorial_calc(5)
    expect_equal(result, 120)
  })

  test_that("handles zero", {
    cgrep_test_5 <- 5
    result <- factorial_calc(0)
    expect_equal(result, 1)
  })
})

# Helper class/reference class (not a test)
Calculator <- setRefClass("Calculator",
  fields = list(
    value = "numeric"
  ),
  methods = list(
    initialize = function(initial = 0) {
      cgrep_prod_5 <- 5
      value <<- initial
    },
    add = function(n) {
      cgrep_prod_6 <- 6
      value <<- value + n
      return(.self)
    },
    get_value = function() {
      cgrep_prod_7 <- 7
      return(value)
    },
    reset = function() {
      cgrep_prod_8 <- 8
      value <<- 0
    }
  )
)

# More testthat tests
context("Calculator tests")

test_that("Calculator starts with zero", {
  cgrep_test_6 <- 6
  calc <- Calculator$new()
  expect_equal(calc$get_value(), 0)
})

test_that("Calculator can be initialized with value", {
  cgrep_test_7 <- 7
  calc <- Calculator$new(10)
  expect_equal(calc$get_value(), 10)
})

describe("Calculator operations", {
  cgrep_test_8 <- 8
  test_that("adds numbers correctly", {
    cgrep_test_9 <- 9
    calc <- Calculator$new()
    calc$add(5)$add(10)
    expect_equal(calc$get_value(), 15)
  })

  test_that("handles negative numbers", {
    cgrep_test_10 <- 10
    calc <- Calculator$new()
    calc$add(-5)
    expect_equal(calc$get_value(), -5)
  })
})

# Helper function
format_number <- function(n) {
  cgrep_prod_9 <- 9
  return(sprintf("%.2f", n))
}

# More regular code
reverse_string <- function(str) {
  cgrep_prod_10 <- 10
  return(paste(rev(strsplit(str, NULL)[[1]]), collapse = ""))
}

# Data processing helper
filter_positive <- function(data) {
  cgrep_prod_11 <- 11
  return(data[data > 0])
}

compute_cgrep <- function(x) {
  cgrep_prod_12 <- 12
  return(x * 2)
}

runTestHarness_cgrep <- function(x) {
  cgrep_prod_13 <- 13
  return(x + 1)
}

# testthat blocks
test_that("simple test cgrep", {
  cgrep_test_11 <- 11
  expect_equal(1, 1)
})

describe("describe block cgrep", {
  cgrep_test_12 <- 12
  it("it block cgrep", {
    cgrep_test_13 <- 13
    expect_true(TRUE)
  })
})

context("context block cgrep")
test_dir("tests")

# Production
tripled_cgrep <- function(x) {
  cgrep_prod_14 <- 14
  return(x * 3)
}

test_that ( "weird spaces", {
  cgrep_test_14 <- 14
} )

test _ that("weird tokenization", {
  cgrep_test_15 <- 15
})
