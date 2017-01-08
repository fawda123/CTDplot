#' Interpolate observed CTD bottom depth along sampling axis
#'
#' Interpolate observed CTD bottom depth along sampling axis
#'
#' @param ctd_pts data.frame of CTD station locations, see details
#' @param expand numeric for expanding sample size of CTD points

#' @details
#' CTD station locations and bottom depths are linearly expanded for use in \code{\link{ctd_plot}} if depth soundings are not provided.  This is done to create a smoother depth gradient in the contour plot.  The \code{ctd_pts} data.frame must have three columns for longitude (\code{Long}), latitude (\code{Latitude}), and station name (\code{Station}).
#'
#' All coordinates are assumed to be geographic decimal degrees using the WGS 1984 projection, negative longitude is west of the Prime Meridian.
#'
#' @return
#' The expanded data.frame from \code{ctd_pts} including depth (\code{Depth}) and cumulative distance (\code{Dist}, in km) columns.
#'
#' @export
#'
#' @import dplyr geosphere
#'
#' @examples
#' \dontrun{
#' dt <- as.Date('2014-04-21')
#' toplo <- ctd[ctd$Date %in% dt, ]
#' get_bdepths(toplo)
#' }
get_bdepths <- function(ctd_pts, expand = 200){

  # get unique locations, max depth at each location
  ctd_pts <- select(ctd_pts, Station, Long, Lat, Depth) %>%
    group_by(Station) %>%
    filter(Depth == max(Depth)) %>%
    ungroup

  # expand value has to be changed to account for adding stations to the interpolated list
  expand <- expand - nrow(ctd_pts) + 2

  # expand ctd points and depth by linear interp
  xint <- stats::approx(x = ctd_pts$Long, n = expand)$y
  yint <- stats::approx(x = ctd_pts$Lat, n = expand)$y
  deps <- stats::approx(x = ctd_pts$Depth, n = expand)$y
  locs <- data.frame(
  	Long = xint,
  	Lat = yint,
    Depth = deps
  	)

  # estimate cumulative distance between points from geosphere (km)
  # extract top part of lower triangle
  dist <- distm(locs[, c('Long', 'Lat')])/1000
  d <- row(dist) - col(dist)
  dist <- split(dist, d)$`1`
  locs$Dist <- c(0, cumsum(dist))

  # get distance between original survey stations
  distorig <- distm(ctd_pts[, c('Long', 'Lat')])/1000
  d <- row(distorig) - col(distorig)
  distorig <- split(distorig, d)$`1`
  ctd_pts$Dist <- c(0, cumsum(distorig))

  # format output
  out <- mutate(locs,
    Station = NA
    ) %>%
    select(Station, Long, Lat, Depth, Dist) %>%
    .[-c(1, nrow(.)), ] %>%
    rbind(., ctd_pts) %>%
    arrange(Dist)

  return(out)

}
