#' Add padding to 3D array
#'
#' `pad_array()` adds additional rows, columns, or slices to a 3-dimensional
#' array, increasing the array's dimensions by the desired amount and filling
#' the new space with a uniform value. It is intended for adding empty or
#' occupied space to the edges of a 3D spatial map.
#'
#' @details `pad_array()` uses the signs of `x`, `y`, and `z` to determine where
#'   to add padding. Negative values are prepended before the array's lower
#'   indices (what one might call the "top", "front", or "beginning" of the
#'   array), while positive values are appended after the upper indices (the
#'   "bottom", "back", or "end" of the array).
#'
#' @param a A 3-dimensional [`array`][array()] of numeric values
#' @param x A positive or negative integer, denoting the number of rows to add
#'   to the array. The sign dictates which side of the array to pad. Default is
#'   zero.
#' @param y A positive or negative integer, denoting the number of columns to
#'   add to the array. The sign dictates which side of the array to pad. Default
#'   is zero.
#' @param z A positive or negative integer, denoting the number of "slices" to
#'   add to the array. The sign dictates which side of the array to pad. Default
#'   is zero.
#' @param fill The desired value to fill the array padding with. Default is
#'   zero.
#'
#' @return A 3-dimensional [`array`][array()] with the desired padding added.
#'   The padded portions are labelled using their [`dimnames`][dimnames()]. If
#'   no padding has been specified, the function returns the original array.
#' @export
#'
#' @examples
#' # generate array
#' a <- array(data = rep(c(1,0), 125), dim = c(5,5,5))
#' # add two rows of zeroes to top of array
#' pad <- pad_array(a, x = -2)
#' # add one row of zeroes to bottom of array, and two columns to beginning
#' pad <- pad_array(a, x = 1, y = -2)
#'
#' @importFrom abind abind
pad_array <- function(a, x = 0, y = 0, z = 0, fill = 0){
  # -------------------------------- Checks ------------------------------------
  # check dimensions of array
  if (length(dim(a)) != 3){
    stop("input array must have 3 dimensions")
  }
  
  # check that array is numeric
  if (!is.numeric(a)){
    stop("input array must be of type 'numeric'")
  }
  
  # check that x,y,z and fill are numeric
  if (any(!is.numeric(c(x, y, z, fill)))){
    stop("inputs for `x`, `y`, `z`, and `fill` must be of type `numeric`")
  }
  
  # check that x,y,z and fill are length 1
  if (any(lengths(list(x, y, z, fill)) != 1)){
    stop("inputs for `x`, `y`, `z`, and `fill` must be of length 1")
  }
  
  # -------------------------------- Padding ----------------------------------- 
  # pad rows (x dimension)
  if (x != 0){
    padding <- array(data = fill,
                     dim = c(abs(x), dim(a)[2], dim(a)[3]),
                     dimnames = list(paste0("padx_",1:abs(x)), NULL, NULL))
    if (x < 0){
      padded <- abind::abind(padding, a, along = 1)} 
    else if (x > 0){
      padded <- abind::abind(a, padding, along = 1)}
  } else {
    padded <- a
  }
  
  # pad columns (y dimension)
  if (y != 0){
    padding <- array(data = fill,
                     dim = c(dim(padded)[1], abs(y), dim(padded)[3]),
                     dimnames = list(NULL, paste0("pady_",1:abs(y)), NULL))
    if (y < 0){
      padded <- abind::abind(padding, padded, along = 2)} 
    else if (y > 0){
      padded <- abind::abind(padded, padding, along = 2)}
  }
  
  # pad slices (z dimension)
  if (z != 0){
    padding <- array(data = fill,
                     dim = c(dim(padded)[1], dim(padded)[2], abs(z)),
                     dimnames = list(NULL, NULL, paste0("padz_",1:abs(z))))
    if (z < 0){
      padded <- abind::abind(padding, padded, along = 3)} 
    else if (z > 0){
      padded <- abind::abind(padded, padding, along = 3)}
  }
  
  return(padded)
}