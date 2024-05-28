#' California oak forest stand before and after 2020 Glass Fire
#'
#' This dataset contains point cloud data from two terrestrial LiDAR scans of a
#' Northern California oak forest shortly before and after the Glass Fire, which
#' burned some 27000 hectares of land in Sonoma and Napa counties between
#' September 27 and October 20, 2020. The scans encompass an identical 24m by
#' 24m rectangular plot at a study site within the Saddle Mountain Open Space
#' Preserve in Sonoma County.
#'
#' The original terrain topography has been removed using digital elevation
#' model (DEM)-based height normalization, and ground points removed by clipping
#' all points below 0.25m. The raw point cloud data were normalized via
#' voxelization at a resolution of 0.125m, and the results further down-sampled
#' to make the dataset more compact. The X, Y, and Z coordinates were generated
#' from the original Easting, Northing, and elevation by subtracting their
#' minimum values.
#'
#' @format A data frame with 1,000,000 rows and 4 columns: `X`, `Y`, `Z`, and
#'   `Year`
#' \describe{
#'  \item{`X`,`Y`,`Z`}{The XYZ spatial positions of each point, in meters. X
#'    and Y denote the East-West and North-South horizontal positions,
#'    respectively, while Z denotes the vertical position}
#'  \item{`Year`}{The year each LiDAR scan was taken, either 2020, immediately
#'    before the Glass Fire, or 2021, a few months after}
#' }
"glassfire"