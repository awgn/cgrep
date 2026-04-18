# R test example file

library(testthat)

# Regular helper function (not a test)
add <- function(a, b) {
  return(a + b)
}

# Another helper function
multiply <- function(x, y) {
  return(x * y)
}

# Helper function
factorial_calc <- function(n) {
  if (n <= 0) {
    return(1)
  }
  return(n * factorial_calc(n - 1))
}

# testthat tests
test_that("addition works correctly", {
  result <- add(2, 3)
  expect_equal(result, 5)
})

test_that("multiplication works", {
  result <- multiply(4, 5)
  expect_equal(result, 20)
})

# Regular function (not a test)
process_data <- function(vec) {
  return(vec[vec > 0] * 2)
}

# testthat describe block
describe("factorial function", {
  test_that("calculates factorial of 5", {
    result <- factorial_calc(5)
    expect_equal(result, 120)
  })
  
  test_that("handles zero", {
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
      value <<- initial
    },
    add = function(n) {
      value <<- value + n
      return(.self)
    },
    get_value = function() {
      return(value)
    },
    reset = function() {
      value <<- 0
    }
  )
)

# More testthat tests
context("Calculator tests")

test_that("Calculator starts with zero", {
  calc <- Calculator$new()
  expect_equal(calc$get_value(), 0)
})

test_that("Calculator can be initialized with value", {
  calc <- Calculator$new(10)
  expect_equal(calc$get_value(), 10)
})

describe("Calculator operations", {
  test_that("adds numbers correctly", {
    calc <- Calculator$new()
    calc$add(5)$add(10)
    expect_equal(calc$get_value(), 15)
  })
  
  test_that("handles negative numbers", {
    calc <- Calculator$new()
    calc$add(-5)
    expect_equal(calc$get_value(), -5)
  })
})

# Helper function
format_number <- function(n) {
  return(sprintf("%.2f", n))
}

# More regular code
reverse_string <- function(str) {
  return(paste(rev(strsplit(str, NULL)[[1]]), collapse = ""))
}

# Data processing helper
filter_positive <- function(data) {
  return(data[data > 0])
}
# =========================================================================
# --- CGREP SEMANTIC TESTS (appended) ---
# =========================================================================

compute_cgrep <- function(x) {
  CGREP_IDENTIFIER <- 1
  return(x * 2)
}

runTestHarness_cgrep <- function(x) {
  CGREP_IDENTIFIER <- 2
  return(x + 1)
}

# testthat blocks
test_that("simple test cgrep", {
  CGREP_IDENTIFIER_TEST <- 1
  expect_equal(1, 1)
})

describe("describe block cgrep", {
  CGREP_IDENTIFIER_TEST <- 2
  it("it block cgrep", {
    CGREP_IDENTIFIER_TEST <- 3
    expect_true(TRUE)
  })
})

# Test execution wrappers (they don't encapsulate logic, so we don't put target identifiers here)
context("context block cgrep")
test_dir("tests")

# Production
tripled_cgrep <- function(x) {
  CGREP_IDENTIFIER <- 3
  return(x * 3)
}

test_that ( "weird spaces", {
  CGREP_IDENTIFIER_TEST <- 4
} )

test _ that("weird tokenization", {
  CGREP_IDENTIFIER_TEST <- 5
})
