#' Create voxel array
#'
#' Converts a table of voxel data into a 3-dimensional array
#'
#' @param x text
#' @param edge_length text
#' @param threshold text
#'
#' @return A 3-dimensional integer [`array`][`array()`] containing values of `0`
#'   or `1`, representing the occupancy of a given voxel
#'
#' @export
#'
#' @examples
#' # simulate a diagonal line of points with XYZ coordinates
#' pc <- data.frame(X = 0:99, Y = 0:99, Z = 0:99)
#' # convert point data to cubic voxels of length 5
#' vox <- voxelize(pc, edge_length = c(5,5,5))
#' # convert to voxel array
#' bounding_box(vox)
bounding_box <- function(x, threshold = 0, edge_length = NULL){
  UseMethod("bounding_box", x)
}

#' @rdname bounding_box
#' @usage NULL
#' @export
bounding_box.default <- function(x, threshold = 0, edge_length = NULL){
  # ------------------------------- Checks -------------------------------------
  
  # check that edge_length is defined
  if (is.null(edge_length)){
    stop("No voxel dimensions specified, please enter a value for 'edge_length'")
  }
  
  # check that the required columns are present
  if (!all(c("X", "Y", "Z", "N") %in% names(x))){
    stop("Required columns not found, input data.table must have columns $X, $Y, $Z, and $N")
  }
  
  # ------------------------- Process data.table -------------------------------
  
  # locally bind variables to allow for clean non-standard evaluation by data.table
  . <- X <- Y <- Z <- N <- Occupied <- NULL
  
  # subset voxel data.table, keeping rows where N is greater than the cutoff,
  # then rounding to hopefully prevent floating-point precision issues, then 
  # removing the N column and adding an occupancy column
  rounded <- x[N > threshold][,(c("X","Y","Z","N","Occupied")) := .(round(X,10), 
                                                                    round(Y,10), 
                                                                    round(Z,10), 
                                                                    NULL,
                                                                    1L)]
  
  # create bounding box and anti-join with the rounded data.table, adding a
  # column for occupancy
  bbox <- data.table(expand.grid(X = round(seq(min(rounded$X),
                                               max(rounded$X),
                                               by = edge_length[[1]]),10),
                                 Y = round(seq(min(rounded$Y),
                                               max(rounded$Y),
                                               by = edge_length[[2]]),10),
                                 Z = round(seq(min(rounded$Z),
                                               max(rounded$Z),
                                               by = edge_length[[3]]),10)))[
                                                 !rounded, on = .(X, Y, Z)
                                               ][, Occupied := 0L]
  
  # concatenate the rounded and bounding box data.tables
  filled <- data.table::rbindlist(list(rounded, bbox))
  
  # reorder the rows to prepare for array conversion
  data.table::setorder(filled, Z, Y, X)
  
  # ----------------------------- Create array ---------------------------------
  
  # convert data.table into 3 dimensional array
  lac_array <- array(data = filled$Occupied, 
                     dim=c(length(unique(filled$X)), 
                           length(unique(filled$Y)), 
                           length(unique(filled$Z))), 
                     dimnames=list(unique(filled$X), 
                                   unique(filled$Y), 
                                   unique(filled$Z)))
  
  # update class?
  
  return(lac_array)
}

#' @rdname bounding_box
#' @usage NULL
#' @export
bounding_box.lac_voxels <- function(x, threshold = 0, edge_length = NULL){
  
  # ------------------------------- Checks -------------------------------------
  
  # check that the required columns are present
  if (!all(c("X", "Y", "Z", "N") %in% names(x))){
    stop("Required columns not found, input data.table must have columns $X, $Y, $Z, and $N")
  }
  
  # ------------------------ Get voxel resolution ------------------------------
  
  # fetch voxel dimensions from the attributes
  res <- attr(x, "voxel_resolution")
  
  # ------------------------- Process data.table -------------------------------

  # locally bind variables to allow for clean non-standard evaluation by data.table
  . <- X <- Y <- Z <- N <- Occupied <- NULL
  
  # subset voxel data.table, keeping rows where N is greater than the cutoff,
  # then rounding to hopefully prevent floating-point precision issues, then 
  # removing the N column and adding an occupancy column
  rounded <- x[N > threshold][,(c("X","Y","Z","N","Occupied")) := .(round(X,10), 
                                                                    round(Y,10), 
                                                                    round(Z,10), 
                                                                    NULL,
                                                                    1L)]
  
  # create bounding box and anti-join with the rounded data.table, adding a
  # column for occupancy
  bbox <- data.table(expand.grid(X = round(seq(min(rounded$X),
                                               max(rounded$X),
                                               by = res[[1]]),10),
                                 Y = round(seq(min(rounded$Y),
                                               max(rounded$Y),
                                               by = res[[2]]),10),
                                 Z = round(seq(min(rounded$Z),
                                               max(rounded$Z),
                                               by = res[[3]]),10)))[
                                                 !rounded, on = .(X, Y, Z)
                                               ][, Occupied := 0L]
  
  # concatenate the rounded and bounding box data.tables
  filled <- data.table::rbindlist(list(rounded, bbox))
  
  # reorder the rows to prepare for array conversion
  data.table::setorder(filled, Z, Y, X)
  
  # ----------------------------- Create array ---------------------------------
  
  # convert data.table into 3 dimensional array
  lac_array <- array(data = filled$Occupied, 
                     dim=c(length(unique(filled$X)), 
                           length(unique(filled$Y)), 
                           length(unique(filled$Z))), 
                     dimnames=list(unique(filled$X), 
                                   unique(filled$Y), 
                                   unique(filled$Z)))
  
  # update class?
  
  return(lac_array)
  
}