# lacunr 1.0.1.9000 (development version)

# lacunr 1.0.1 (Release date: 2024-06-03)

## New features:

* New plotting functions `lac_plot()`, `lacnorm_plot()`, and `hr_plot()` now let users conveniently plot lacunarity or H(r) curves returned by `lacunarity()`.
* Package now has a sample LiDAR dataset, `glassfire`.
* Package now includes tutorial vignettes.
* Package now has a working test suite.

## Improvements:

* Package is now set up for CI via Github Actions.
* Minor tweaks to `lacunarity()`:
    - The `periodic` argument now has improved error handling.
    - The underlying C++ function received a minor change to simplify how it calculates lacunarity at the smallest box size. This shouldn't alter the output.
* Improvements to error handling of `bounding_box()` and `voxelize()`.
* Removed C++11 requirement from `Makevars[.win]` to make the package compatible with R version 4.3. This may cause issues building the package with R versions <= 3.5 on systems with very old compilers.
* Various bug fixes in the package documentation to satisfy `R CMD check` --- mainly issues involving formatting of DOIs and removing broken URLs.

# lacunr 0.2.0 (Release date: 2024-03-11)

## New features:

* New function `pad_array()` added.
* `lacunarity()` now computes H(r) curves in addition to lacunarity values.

# lacunr 0.1.1 (Release date: 2023-12-11)

## Initial public launch

* Core functions `voxelize()`, `bounding_box()`, and `lacunarity()` implemented.
* `bounding_box()` includes S3 method for `lasmetrics3d` class created by `lidR::voxel_metrics()`, as well as `lacunr`'s native `lac_voxels` class created by `voxelize()`.
