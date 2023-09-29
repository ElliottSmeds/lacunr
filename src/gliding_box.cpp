// A script for performing gliding box lacunarity calculation for 3d voxel arrays
// using the integral image algorithm (Williams 2015)

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;


// gliding box algorithm
// [[Rcpp::export(.gliding_box)]]
DataFrame gliding_box(arma::ucube C, IntegerVector box_sizes) {
  
  // define dimensions of array
  int Xdim = C.n_rows;
  int Ydim = C.n_cols;
  int Zdim = C.n_slices;
  
  // initialize vectors for box size and lacunarity
  arma::uvec sizes(box_sizes.size());
  arma::vec lacunarity(box_sizes.size());
  
  // initialize integral image array (encoded as doubles to allow for large integers)
  // (element values are zero by default)
  arma::dcube int_img(Xdim+1,Ydim+1,Zdim+1);
  
  // traverse the voxel array to compute sums for integral image
  // loop through Z values
  for (unsigned int z=1; z<=Zdim; ++z){
    // loop through Y values
    for (unsigned int y=1; y<=Ydim; ++y){
      // loop through X values
      for (unsigned int x=1; x<=Xdim; ++x){
        // find the summed volume for a given element of the integral image array
        int_img(x,y,z) = (double) C(x-1,y-1,z-1) + int_img(x-1,y-1,z-1) + 
          int_img(x,y,z-1) + int_img(x,y-1,z) + int_img(x-1,y,z) - 
          int_img(x-1,y-1,z) - int_img(x-1,y,z-1) - int_img(x,y-1,z-1);
      }
    }
  }
  
  // We get the smallest box size for free by summing the whole array
  double Freq = arma::accu(C);
  double S1S2 = Freq / C.n_elem;
  double lac1 = S1S2 / (S1S2 * S1S2);
  // output box size and lacunarity to vectors
  sizes(0) = 1;
  lacunarity(0) = lac1;
  
  // loop through remaining box sizes to calculate lacunarity
  for (int i=1; i<box_sizes.size(); i++) {
    // define box size
    unsigned int box_size = box_sizes(i);
    
    // define the number of box positions for each dimension
    unsigned int Xnum = Xdim-box_size;
    unsigned int Ynum = Ydim-box_size;
    unsigned int Znum = Zdim-box_size;
    
    unsigned int Xend = Xdim-Xnum;
    unsigned int Yend = Ydim-Ynum;
    unsigned int Zend = Zdim-Znum;
    
    // Compute the box mass array by adding/subtracting corners of integral image
    arma::dcube M = int_img(arma::span(Xend,Xdim),
                            arma::span(Yend,Ydim),
                            arma::span(Zend,Zdim)) -
                      int_img(arma::span(0,Xnum),
                              arma::span(Yend,Ydim),
                              arma::span(Zend,Zdim)) -
                        int_img(arma::span(Xend,Xdim),
                                arma::span(0,Ynum),
                                arma::span(Zend,Zdim)) -
                          int_img(arma::span(Xend,Xdim),
                                  arma::span(Yend,Ydim),
                                  arma::span(0,Znum)) +
                            int_img(arma::span(0,Xnum),
                                    arma::span(Yend,Ydim),
                                    arma::span(0,Znum)) +
                              int_img(arma::span(0,Xnum),
                                      arma::span(0,Ynum),
                                      arma::span(Zend,Zdim)) +
                                int_img(arma::span(Xend,Xdim),
                                        arma::span(0,Ynum),
                                        arma::span(0,Znum)) -
                                  int_img(arma::span(0,Xnum),
                                          arma::span(0,Ynum),
                                          arma::span(0,Znum))    
      ;
    
    // calculate lacunarity from box mass array
    double boxes = M.n_elem;
    double numerator = arma::accu(M % M);
    double denominator = arma::accu(M);
    double Lac = boxes * numerator / (denominator * denominator);
    
    // output box size and lacunarity to vectors
    sizes(i) = box_size;
    lacunarity(i) = Lac;
  }
  // return dataframe of box sizes and mass vectors
  DataFrame lac_curve = 
    DataFrame::create(Named("box_size") = sizes,
                      Named("lacunarity") = lacunarity);
  return lac_curve;
}
