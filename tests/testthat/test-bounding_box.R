test_that("invalid 'edge_length' throws error", {
  vox <- data.table(X = 1:3, Y = 1:3, Z = 1:3, N = c(1,1,1))
  expect_error(bounding_box(vox), "No voxel dimensions found")
  expect_error(bounding_box(vox, edge_length = c("1","1","1")), "must be a numeric vector")
  expect_error(bounding_box(vox, edge_length = list(1,1,1)), "must be a numeric vector")
  expect_error(bounding_box(vox, edge_length = 1), "must be a vector of length 3")
})

test_that("missing columns throw error", {
  vox <- data.table(X = 1:3, Y = 1:3, Z = 1:3)
  expect_error(bounding_box(vox, edge_length = c(1,1,1)), "Required columns not found")
})

test_that("invalid 'threshold' throws error", {
  pc <- data.table(X = 1:3, Y = 1:3, Z = 1:3)
  vox <- voxelize(pc, edge_length = c(1,1,1))
  expect_error(bounding_box(vox, threshold = NULL), "must be a single numeric value")
  expect_error(bounding_box(vox, threshold = "foo"), "must be a single numeric value")
  expect_error(bounding_box(vox, threshold = 1:3), "must be a single numeric value")
})

test_that("invalid 'lac_voxels' throws error", {
  pc <- data.table(X = 1:3, Y = 1:3, Z = 1:3)
  vox <- voxelize(pc, edge_length = c(1,1,1))
  attr(vox, "voxel_resolution") <- c("1","1","1")
  expect_error(bounding_box(vox), "improperly formatted attribute")
  attr(vox, "voxel_resolution") <- c(1,1,1,1)
  expect_error(bounding_box(vox), "improperly formatted attribute")
})

test_that("invalid 'lasmetrics3d' throws error", {
  skip_if_not_installed("lidR")
  suppressMessages(las <- lidR::LAS(data.table(X = as.numeric(1:3), 
                                               Y = as.numeric(1:3), 
                                               Z = as.numeric(1:3))))
  vox <- lidR::voxel_metrics(las, ~list(N = length(Z)), res = c(1,2))
  attr(vox, "res") <- "1"
  expect_error(bounding_box(vox), "improperly formatted attribute")
  attr(vox, "res") <- c(1,2)
  expect_error(bounding_box(vox), "improperly formatted attribute")
})

test_that("'lac_voxels' gives accurate output", {
  pc <- data.table(X = 1:3, Y = 1:3, Z = 1:3)
  vox <- voxelize(pc, edge_length = c(1,1,1))
  exp <- array(c(1, rep(0, 12)), 
               dim = c(3,3,3),
               dimnames = rep(list(1.5:3.5), 3))
  expect_equal(bounding_box(vox), exp)
})

test_that("'lasmetrics3d' gives accurate output", {
  skip_if_not_installed("lidR")
  suppressMessages(las <- lidR::LAS(data.table(X = as.numeric(1:3), 
                                               Y = as.numeric(1:3), 
                                               Z = as.numeric(1:3))))
  vox <- lidR::voxel_metrics(las, ~list(N = length(Z)), 1)
  exp <- array(c(1, rep(0, 12)), 
               dim = c(3,3,3),
               dimnames = list(1.5:3.5, 1.5:3.5, 1:3))
  expect_equal(bounding_box(vox), exp)
})
