test_that("wrong dimensions produce error", {
  expect_error(lacunarity(array(data = 1, dim = c(2,2))), "must have 3 dimensions")
  expect_error(lacunarity(array(data = 1, dim = c(2,2,2,2))), "must have 3 dimensions")
})

test_that("non-numeric array produces error", {
  expect_error(lacunarity(array(data = "1", dim = c(2,2,2))),"array must be of type 'numeric'")
})

test_that("NAs produce error", {
  expect_error(lacunarity(array(data = c(NA, 1), dim = c(2,2,2))),
               "contains NA values")
})

test_that("invalid 'box sizes' argument produces error", {
  a <- array(data = 1, dim = c(2,2,2))
  expect_error(lacunarity(a, box_sizes = NULL),"invalid input for 'box_sizes'")
  expect_error(lacunarity(a, box_sizes = "foo"),"invalid input for 'box_sizes'")
  expect_error(lacunarity(a, box_sizes = data.frame()),"invalid input for 'box_sizes'")
  expect_error(lacunarity(a, box_sizes = c("1","2","3")),"must be of type 'numeric'")
  expect_error(lacunarity(a, box_sizes = 0:2),"contain zero or negative integers")
  expect_warning(lacunarity(a, box_sizes = 2:3),"omits 1")
})

test_that("invalid 'periodic' argument produces error", {
  a <- array(data = 1, dim = c(2,2,2))
  expect_error(lacunarity(a, periodic = NULL),"invalid input for 'periodic'")
  expect_error(lacunarity(a, periodic = 6),"invalid input for 'periodic'")
  expect_error(lacunarity(a, periodic = "foo"),"invalid input for 'periodic'")
  expect_error(lacunarity(a, periodic = data.frame(x = TRUE)),"invalid input for 'periodic'")
  expect_error(lacunarity(a, periodic = c(TRUE, FALSE)),"invalid input for 'periodic'")
})

test_that("excess box sizes are ignored", {
  a <- array(data = 1, dim = c(3,3,3))
  expect_identical(lacunarity(a, box_sizes = 1:3), lacunarity(a, box_sizes = 1:5))
})

test_that("lacunarity values are accurate", {
  lac <- lacunarity(array(data = c(0,1), dim = c(5,5,5)), box_sizes = "all")
  expect_equal(lac$lacunarity, c(125/62, 1, 132678/132496, 1, 1))
})
