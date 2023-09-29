#' Gliding-box lacunarity
#'
#' @param x a 3-dimensional [`array`][array()] of integer values
#' @param box_sizes Which box sizes to use for calculating lacunarity:
#'   * "twos" returns box sizes for all powers of two less
#'   than or equal to the smallest dimension of `x`.
#'   * "all" calculates every possible box size up to the smallest dimension of `x`.
#'
#' @return a [`data.frame`][data.frame()] containing box sizes and their
#'   corresponding lacunarity values
#' @export
#'
#' @examples
#' a <- array(data = rep(c(1,0), 125), dim = c(5,5,5))
#' lacunarity(a)

lacunarity <- function(x, box_sizes = "twos"){

  # measure dimensions of the array
  Xdim <- dim(x)[1]
  Ydim <- dim(x)[2]
  Zdim <- dim(x)[3]
  
  # find the smallest dimension
  Smallest <- min(Xdim,Ydim,Zdim)
  
  # generate vector of box sizes
  if (box_sizes == "twos"){
    # find the largest power of two that is less than Smallest
    last2 <- floor(log2(Smallest))
    
    # generate vector of powers of two
    sizes <- 2^(0:last2)
    
  } else if (box_sizes == "all"){
    # generate vector of all possible box sizes
    sizes <- 1:Smallest
    
  } else {
    stop("invalid input for 'box_sizes'")
  }
  
  # calculate lacunarity for all box sizes
  lac_curve <- .gliding_box(C = x, box_sizes = sizes)
  
  return(lac_curve)
}