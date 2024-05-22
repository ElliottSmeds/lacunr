test_that("non-dataframe throws error", {
  mat <- matrix(1:3, 3, 4, dimnames = list(NULL, c("box_size", 
                                                   "lacunarity", 
                                                   "lac_norm", 
                                                   "H_r")))
  df <- data.frame(box_size = 1:3, 
                   lacunarity = 1:3, 
                   lac_norm = 1:3, 
                   H_r = 1:3)
  expect_error(lac_plot(mat, df), "must be a data.frame")
  expect_error(lacnorm_plot(mat, df), "must be a data.frame")
  expect_error(hr_plot(mat, df), "must be a data.frame")
  expect_no_error(lac_plot(as.data.frame(mat)))
})

test_that("missing columns throws error", {
  df <- data.frame(box_size = 1:3, lacunarity = 1:3, lac_norm = 1:3, H_r = 1:3)
  expect_error(lac_plot(df[,-2]), "required columns")
  expect_error(lacnorm_plot(df[,-3]), "required columns")
  expect_error(hr_plot(df[,-4]), "required columns")
})

test_that("non-character group_names throws error", {
  df1 <- data.frame(box_size = 1:3, lacunarity = 1:3, lac_norm = 1:3, H_r = 1:3)
  expect_error(lac_plot(df1, group_names = c(1,2)), "must be a character vector")
  expect_error(lacnorm_plot(df1, group_names = c(1,2)), "must be a character vector")
  expect_error(hr_plot(df1, group_names = c(1,2)), "must be a character vector")
})

test_that("wrong group_names length throws error", {
  df1 <- data.frame(box_size = 1:3, lacunarity = 1:3, lac_norm = 1:3, H_r = 1:3)
  expect_error(lac_plot(df1, group_names = c("one", "two")), "does not match the number")
  expect_error(lacnorm_plot(df1, group_names = c("one", "two")), "does not match the number")
  expect_error(hr_plot(df1, group_names = c("one", "two")), "does not match the number")
})

test_that("ellipsis is parsed into group labels", {
  df1 <- data.frame(box_size = 1:3, lacunarity = 1:3, lac_norm = 1:3, H_r = 1:3)
  df2 <- data.frame(box_size = 1:3, lacunarity = 1:3, lac_norm = 1:3, H_r = 1:3)
  lac <- lac_plot(df1, df2)
  expect_equal(levels(lac$data$Source), c("df1","df2"))
  lac <- lacnorm_plot(df1, df2)
  expect_equal(levels(lac$data$Source), c("df1","df2"))
  lac <- hr_plot(df2, df1)
  expect_equal(levels(lac$data$Source), c("df2","df1"))
})
