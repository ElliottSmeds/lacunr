test_that("not a dataframe throws error", {
  expect_error(voxelize("foo", edge_length = c(1,1,1)), "must be a data.frame")
  expect_error(voxelize(1:3, edge_length = c(1,1,1)), "must be a data.frame")
  expect_error(voxelize(list(X = 1, Y = 1, Z = 1), edge_length = c(1,1,1)), "must be a data.frame")
})

test_that("missing columns throw error", {
  expect_error(voxelize(data.frame(), edge_length = c(1,1,1)), "Required columns not found")
  expect_error(voxelize(data.frame(X = 1, Y = 1, z = 1), edge_length = c(1,1,1)), 
               "Required columns not found")
})

test_that("non-numeric columns throw error", {
  expect_error(voxelize(data.frame(X = 1, Y = "2", Z = 3), edge_length = c(1,1,1)),
               "must be of type 'numeric'")
})

test_that("wrong 'edge_length' throws error", {
  pc <- data.frame(X = 1:3, Y = 1:3, Z = 1:3)
  expect_error(voxelize(pc, edge_length = c("1","1","1")), "must be a numeric vector")
  expect_error(voxelize(pc, edge_length = data.frame()), "must be a numeric vector")
  expect_error(voxelize(pc, edge_length = 4), "must be a vector of length 3")
  expect_error(voxelize(pc, edge_length = rep(1,4)), "must be a vector of length 3")
  expect_error(voxelize(pc, edge_length = c(1,1,-1)), "must be greater than zero")
})

test_that("wrong 'threads' throws error", {
  pc <- data.frame(X = 1:3, Y = 1:3, Z = 1:3)
  res <- c(1,1,1)
  expect_error(voxelize(pc, res, threads = 1:3), "must be a single numeric value")
  expect_error(voxelize(pc, res, threads = data.frame()), "must be a single numeric value")
  expect_error(voxelize(pc, res, threads = -1), "cannot be negative or zero")
})

test_that("function output is accurate", {
  pc <- data.frame(X = c(0.5,1,2,3), Y = c(1,0.5,2,3), Z = rep(2.5, 4))
  exp <- data.frame(X = 1:3, Y = 1:3, Z = rep(3,3), N = c(2,1,1))
  attr(exp, 'class') <- c("lac_voxels", "data.table", "data.frame")
  attr(exp, 'voxel_resolution') <- c(1,1,1)
  expect_equal(voxelize(pc, edge_length = c(1,1,1)), exp)
})
