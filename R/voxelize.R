#' Voxelize point cloud
#'
#' Bins point cloud data into 3D pixels, otherwise known as 'voxels'.
#'
#' @param x A [`data.frame`][data.frame()] or [`data.table`][`data.table()`]
#'   with columns containing the X, Y, and Z coordinates of every point. Any
#'   additional columns are ignored.
#' @param edge_length A numeric [`vector`][vector()] of length `3`, containing
#'   values for the X, Y, and Z dimensions of each voxel.
#' @param threads The number of threads to use for computing the voxel data.
#'   Default is 1.
#'
#' @return A data object of class '`lac_voxels`', which inherits from
#'   [`data.table`][`data.table()`]. The output contains 4 columns: X, Y, Z, and
#'   N. The first three columns encode the spatial coordinates of each voxel
#'   while the fourth denotes the total number of points they contain.
#' @export
#'
#' @examples
#' # simulate a diagonal line of points with XYZ coordinates
#' pc <- data.frame(X = 0:99, Y = 0:99, Z = 0:99)
#' # convert point data to cubic voxels of length 5
#' voxelize(pc, edge_length = c(5,5,5))
#' 
voxelize <- function(x, edge_length, threads = 1L) {
  # -------------------------------- Checks ------------------------------------
  # check that the required columns are present
  if (!all(c("X", "Y", "Z") %in% names(x))){
    stop("Required columns not found, input data must have columns $X, $Y, and $Z")
  }
  
  # check that the first XYZ columns are numeric
  if (!is.numeric(as.matrix(x[,c("X", "Y", "Z")]))){
    stop("point cloud data must be of type 'numeric'")
  }
  
  # check that edge_length is numeric and of length 3
  if (!is.numeric(edge_length)){
    stop("'edge_length' argument must be of type 'numeric'")
  } 
  else if (length(edge_length) != 3){
    stop("'edge_length' argument must be a vector of length 3")
  }
  
  # check that threads value is numeric
  if(!is.numeric(threads)){
    stop("'threads' argument must be of type 'numeric'")
  }
  
  # ------------------------------- Voxelize -----------------------------------
  # locally bind variables to allow for clean non-standard evaluation by data.table
  . <- X <- Y <- Z <- NULL
  
  # convert point-cloud to matrix and send to C++ function to round XYZ values
  vox <- as.data.table(.voxelize_C(as.matrix(x[,c("X", "Y", "Z")]), edge_length, threads))
  
  # add names to the returned data table
  colnames(vox) <- c("X", "Y", "Z")
  
  # count the number of points per voxel
  vox <- vox[ , .N, by = .(X, Y, Z)]
  
  # add the voxel dimensions as a new attribute and update the class
  data.table::setattr(vox, "voxel_resolution", edge_length)
  data.table::setattr(vox, "class", c("lac_voxels", attr(vox, "class")))
  
  return(vox)
}
