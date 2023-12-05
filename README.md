
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lacunr

<!-- badges: start -->
<!-- badges: end -->

`lacunr` is an R package for calculating 3D lacunarity from voxel data.
It is designed to be used with LiDAR point clouds to measure the
heterogeneity or “gappiness” of 3-dimensional structures such as forest
stands.

## Installation

You can install the development version of lacunr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("ElliottSmeds/lacunr")
```

## Basic Usage

The standard workflow for `lacunr` is fairly simple:

1.  Convert point cloud data to voxels using `voxelize()`
2.  Arrange the voxels into a 3-dimensional array using `bounding_box()`
3.  Calculate a lacunarity curve using `lacunarity()`

``` r
library(lacunr)
# create a data.frame of simulated point cloud data
pc <- data.frame(X = rnorm(1000, 10), Y = rnorm(1000, 50), Z = rnorm(1000, 25))
# convert to voxels of size 0.5
vox <- voxelize(pc, edge_length = c(0.5, 0.5, 0.5))
# generate 3-dimensional array
box <- bounding_box(vox)
# calculate lacunarity curve
lac_curve <- lacunarity(box)
```
