// A C++ script for voxelizing LiDAR point cloud data
// Adapted by Elliott A. Smeds from code authored by J. Antonio Guzmán Q. for 
// the R package 'rTLS', under the terms of the GPL-3.0 open software license

#include <RcppArmadillo.h>
#include <RcppThread.h>

// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::depends(RcppThread)]]

using namespace arma;

// [[Rcpp::export(.voxelize_C)]]
arma::mat voxelization_rcpp(arma::mat pc, arma::vec edge_length, int threads = 1) {

  // get minimum values for each spatial dimension
  double xmin = min(pc.col(0));
  double ymin = min(pc.col(1));
  double zmin = min(pc.col(2));
  
  // get number of rows in the point cloud matrix
  unsigned long int nrowspc = pc.n_rows;
  
  // initialize voxel matrix with 3 columns
  arma::mat voxels(nrowspc, 3);
  
  // define the voxel edge lengths
  double edge_x = edge_length[0];
  double edge_y = edge_length[1];
  double edge_z = edge_length[2];
  
  // loop through each row of the point cloud matrix and round each spatial 
  // dimension to the nearest voxel bin
  RcppThread::parallelFor(0, nrowspc, [&] (unsigned long int i) {
    
    int xvox = floor(((pc(i, 0) - xmin)/edge_x));
    int yvox = floor(((pc(i, 1) - ymin)/edge_y));
    int zvox = floor(((pc(i, 2) - zmin)/edge_z));
    
    voxels(i, 0) = xmin + (xvox*edge_x) + (edge_x/2);
    voxels(i, 1) = ymin + (yvox*edge_y) + (edge_y/2);
    voxels(i, 2) = zmin + (zvox*edge_z) + (edge_z/2);
    
  }, threads);
 
  return voxels;
}
