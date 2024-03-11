// A script for performing gliding box lacunarity calculation for 3d voxel arrays
// using the integral image algorithm (Williams 2015)
// Author: Elliott A. Smeds

#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

// gliding box -- fixed boundary algorithm (Allain & Cloitre)
// [[Rcpp::export(.gliding_box)]]
DataFrame gliding_box(arma::ucube C, IntegerVector box_sizes) {
  
  // define dimensions of array
  unsigned int Xdim = C.n_rows;
  unsigned int Ydim = C.n_cols;
  unsigned int Zdim = C.n_slices;
  
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
    
    // define the boundaries of the corner sub-arrays
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
    double boxes = M.n_elem; // the number of elements in the array
    double numerator = arma::accu(M % M); // the sum of the squared box masses
    double denominator = arma::accu(M); // the sum of the box masses (will be squared in next step)
    double Lac = boxes * numerator / (denominator * denominator);
    
    // output box size and lacunarity to vectors
    sizes(i) = box_size;
    lacunarity(i) = Lac;
  }
  
  // calculate normalized lacunarity
  arma::vec lacnorm = arma::log(lacunarity) / log(lac1);
  
  // find the smallest dimension of the input array
  double L = (double) std::min<unsigned int>({Xdim,Ydim,Zdim});
  
  // calculate Brownian decay (a diagonal line) for H(r) calculation
  // (requires converting box size vector from integers to doubles)
  arma::vec dsize = arma::conv_to<arma::dvec>::from(sizes);
  arma::vec diag = -(arma::log(dsize))/log(L) + 0.5;
  
  // compute H(r) by subtracting diagonal line from normalized lacunarity curve
  arma::vec Hr = lacnorm - diag;
  
  // return dataframe of box sizes and lacunarity curves
  DataFrame lac_curve = 
    DataFrame::create(Named("box_size") = sizes,
                      Named("lacunarity") = lacunarity,
                      Named("lac_norm") = lacnorm,
                      Named("H_r") = Hr);
  return lac_curve;
}

// gliding box -- periodic boundary algorithm (Feagin et al 2006)
// [[Rcpp::export(.gliding_box_periodic)]]
DataFrame gliding_box_periodic(arma::ucube C, IntegerVector box_sizes) {
  
  // define dimensions of array
  unsigned int Xdim = C.n_rows;
  unsigned int Ydim = C.n_cols;
  unsigned int Zdim = C.n_slices;
  
  // initialize vectors for box size and lacunarity
  arma::uvec sizes(box_sizes.size());
  arma::vec lacunarity(box_sizes.size());
  
  // initialize expanded integral image array 
  // (encoded as doubles to allow for large integers)
  // (element values are zero by default)
  arma::dcube int_img(Xdim*2,Ydim*2,Zdim*2);
  
  // traverse the voxel array to compute sums for integral image
  // loop through Z values
  for (unsigned int z=1; z<Zdim*2; ++z){
    // loop through Y values
    for (unsigned int y=1; y<Ydim*2; ++y){
      // loop through X values
      for (unsigned int x=1; x<Xdim*2; ++x){
        // find the summed volume for a given element of the integral image array
        int_img(x,y,z) = (double) C((x-1)%Xdim,(y-1)%Ydim,(z-1)%Zdim) + int_img(x-1,y-1,z-1) + 
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
    
    // define the boundaries of the corner sub-arrays
    unsigned int subX = Xdim-box_size+1;
    unsigned int subY = Ydim-box_size+1;
    unsigned int subZ = Zdim-box_size+1;
    
    unsigned int Xmid = Xdim-subX+1;
    unsigned int Ymid = Ydim-subY+1;
    unsigned int Zmid = Zdim-subZ+1;
    
    unsigned int Xend = Xdim+box_size-1;
    unsigned int Yend = Ydim+box_size-1;
    unsigned int Zend = Zdim+box_size-1;
    
    // Compute the box mass array by adding/subtracting corners of integral image
    arma::dcube M = int_img(arma::span(Xmid,Xend),
                            arma::span(Ymid,Yend),
                            arma::span(Zmid,Zend)) -
                      int_img(arma::span(0,Xdim-1),
                              arma::span(Ymid,Yend),
                              arma::span(Zmid,Zend)) -
                        int_img(arma::span(Xmid,Xend),
                                arma::span(0,Ydim-1),
                                arma::span(Zmid,Zend)) -
                          int_img(arma::span(Xmid,Xend),
                                  arma::span(Ymid,Yend),
                                  arma::span(0,Zdim-1)) +
                            int_img(arma::span(0,Xdim-1),
                                    arma::span(Ymid,Yend),
                                    arma::span(0,Zdim-1)) +
                              int_img(arma::span(0,Xdim-1),
                                      arma::span(0,Ydim-1),
                                      arma::span(Zmid,Zend)) +
                                int_img(arma::span(Xmid,Xend),
                                        arma::span(0,Ydim-1),
                                        arma::span(0,Zdim-1)) -
                                  int_img(arma::span(0,Xdim-1),
                                          arma::span(0,Ydim-1),
                                          arma::span(0,Zdim-1))    
      ;
    
    // calculate lacunarity from box mass array
    double boxes = M.n_elem; // the number of elements in the array
    double numerator = arma::accu(M % M); // the sum of the squared box masses
    double denominator = arma::accu(M); // the sum of the box masses (will be squared in next step)
    double Lac = boxes * numerator / (denominator * denominator);
    
    // output box size and lacunarity to vectors
    sizes(i) = box_size;
    lacunarity(i) = Lac;
  }
  
  // calculate normalized lacunarity
  arma::vec lacnorm = arma::log(lacunarity) / log(lac1);
  
  // find the smallest dimension of the input array
  double L = (double) std::min<unsigned int>({Xdim,Ydim,Zdim});
  
  // calculate Brownian decay (a diagonal line) for H(r) calculation
  // (requires converting box size vector from integers to doubles)
  arma::vec dsize = arma::conv_to<arma::dvec>::from(sizes);
  arma::vec diag = -(arma::log(dsize))/log(L) + 0.5;
  
  // compute H(r) by subtracting diagonal line from normalized lacunarity curve
  arma::vec Hr = lacnorm - diag;
  
  // return dataframe of box sizes and lacunarity curves
  DataFrame lac_curve = 
    DataFrame::create(Named("box_size") = sizes,
                      Named("lacunarity") = lacunarity,
                      Named("lac_norm") = lacnorm,
                      Named("H_r") = Hr);
  return lac_curve;
}
