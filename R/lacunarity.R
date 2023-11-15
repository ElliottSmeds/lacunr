#' Calculate gliding-box lacunarity
#'
#' Generates Λ(r) lacunarity curves for a specified set of box sizes, using one
#' of two versions of the gliding-box algorithm
#'
#' @details The raw Λ(r) values depend on the proportion of occupied voxels
#'   within the data space. As a result, it is difficult to compare two spatial
#'   patterns with different occupancy proportions because the curves will begin
#'   at different y-intercepts. This is rectified by normalizing the curve,
#'   typically by log-transforming it and dividing by the lacunarity value at
#'   the smallest box size (i.e. `log(Λ(r))/log(Λ(1))`).
#'
#' @param x A 3-dimensional [`array`][array()] of integer values
#' @param box_sizes Which box sizes to use for calculating lacunarity:
#'   * `"twos"` (the default) returns box sizes for all powers of two less than or
#'   equal to the smallest dimension of `x`.
#'   * `"all"` calculates every possible box size up to the smallest dimension of `x`.
#'   * Alternatively, users may supply their own [`vector`][vector()] of custom
#'   box sizes. This vector must be of type "`numeric`" and can only contain
#'   positive values. Values which exceed the dimensions of `x` are ignored.
#' @param periodic A Boolean. Determines which boundary algorithm to use, the
#'   classic fixed boundary (default) or the periodic boundary algorithm
#'   introduced by Feagin et al. 2006. The latter is slightly slower but is more
#'   robust to edge effects.
#'
#' @return A [`data.frame`][data.frame()] containing box sizes and their
#'   corresponding raw and normalized Λ(r) lacunarity values. Lacunarity is
#'   always computed for box size 1, even if the user supplies a custom
#'   `box_sizes` vector that omits it, as this value is required to calculate
#'   normalized lacunarity.
#' @export
#'
#' @examples
#' # generate array
#' a <- array(data = rep(c(1,0), 125), dim = c(5,5,5))
#' # calculate lacunarity with default options
#' lacunarity(a)
#' # supply custom vector of box sizes
#' lacunarity(a, box_sizes = c(1,3,5))
#' # calculate lacunarity at all box sizes using the periodic boundary algorithm
#' lacunarity(a, box_sizes = "all", periodic = TRUE)

lacunarity <- function(x, 
                       box_sizes = "twos",
                       periodic = FALSE)
  {
  
  # ------------------------------ Check array ---------------------------------
  # check dimensions of array
  if (length(dim(x)) != 3){
    stop("input array must have 3 dimensions")
  }
  
  # check that array is numeric
  if (!is.numeric(x)){
    stop("input array must be of type 'numeric'")
  }
  
  # check for NAs
  if (any(is.na(x))){
    stop("input array contains NA values")
  }

  # -------------------------- Determine array size ----------------------------
  # measure dimensions of the array
  Xdim <- dim(x)[1]
  Ydim <- dim(x)[2]
  Zdim <- dim(x)[3]
  
  # find the smallest dimension
  Smallest <- min(Xdim,Ydim,Zdim)
  
  # --------------------- Generate vector of box sizes -------------------------
  if (length(box_sizes) == 1){  
    # Option 1 (default) -- "powers of two"
    if (box_sizes == "twos"){
      # find the largest power of two that is less than Smallest
      last2 <- floor(log2(Smallest))
      
      # generate vector of powers of two
      sizes <- 2^(0:last2)
    } 
    
    # Option 2 -- "all box sizes"
    else if (box_sizes == "all"){
      # generate vector of all possible box sizes
      sizes <- 1:Smallest
    }
    
    # stop if any other input
    else {
      stop("invalid input for 'box_sizes', see ?lacunarity() for valid options")}
  } 
  
  # Option 3 -- custom vector of box size integers
  else if (is.vector(box_sizes, mode = "numeric")){
    
    # stop if the vector contains zeroes or negative values
    if (sort(box_sizes)[[1]] < 1){
      stop("'box_sizes' vector cannot contain zero or negative integers")
    }
    # prepend a 1 if the vector does not already contain it (removing
    # duplicate values or values that exceed dimensions of array)
    else if (sort(box_sizes)[[1]] > 1){
      sizes <- c(1, sort(unique(box_sizes[box_sizes<=Smallest])))
      warning("'box_sizes' vector omits 1, adding additional element to vector")
    }
    # or else pass the unaltered vector (removing duplicate values or values
    # that exceed dimensions of array)
    else if (sort(box_sizes)[[1]] == 1){
      sizes <- sort(unique(box_sizes[box_sizes<=Smallest]))
    }
  }
  
  # stop if the box_sizes vector is not a numeric vector
  else if (is.vector(box_sizes) && !is.vector(box_sizes, mode = "numeric")){
    stop("custom 'box_sizes' vector must be of type 'numeric'")
  }
  
  # stop for any other value of box_sizes
  else {
    stop("invalid input for 'box_sizes', see ?lacunarity() for valid options")
  }
  
  # -------------------------- Calculate lacunarity ----------------------------
  # pass the array and box_sizes vector to the desired C++ function
  if (periodic == FALSE){
    lac_curve <- .gliding_box(C = x, box_sizes = sizes)
  }
  
  else if (periodic == TRUE){
    lac_curve <- .gliding_box_periodic(C = x, box_sizes = sizes)
  }
  
  else {
    stop("invalid input for 'periodic', see ?lacunarity() for valid options")
  }
  
  return(lac_curve)
}
