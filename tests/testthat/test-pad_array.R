test_that("non-3d-array throws error", {
  expect_error(pad_array(data.frame()), "must be a 3-dimensional array")
  expect_error(pad_array(1:3), "must be a 3-dimensional array")
  expect_error(pad_array(matrix(1, 2, 2)), "must be a 3-dimensional array")
})

test_that("non-numeric array throws error", {
  expect_error(pad_array(array("1", c(1,1,1))), "must be of type 'numeric'")
})

test_that("non-numeric x/y/z/fill throw error", {
  a <- array(1, c(1,1,1))
  expect_error(pad_array(a, x = "1", y = 1, z = 1), "must be of type 'numeric'")
  expect_error(pad_array(a, fill = "foo"), "must be of type 'numeric'")
})

test_that("output is accurate", {
  a <- array(1, c(1,1,1), list("one", "two", "three"))
  exp <- array(c(0,0,1,0,0,0,0,0), c(2,2,2), list(c("one","padx_1"),
                                                  c("pady_1", "two"),
                                                  c("three", "padz_1")))
  expect_equal(pad_array(a, x = 1, y = -1, z = 1), exp)
})
