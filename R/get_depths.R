#' Match CTD points with closest hydro soundings
#'
#' Match CTD points with closest hydro soundings
#'
#' @param ctd_pts data.frame of CTD station locations, see details
#' @param dep_pts data.frame of depth sounding points, see details
#' @param expand numeric for expanding sample size of CTD points
#' @param plot logical to return plot of matched points
#' @param show_bath logical to show bathymetric depth points on the plot
#' @param zoom numeric for map zoom, passed to \code{\link[ggmap]{get_stamenmap}}
#'
#' @details
#' CTD station locations are linearly expanded and then matched to the nearests points in the depth sounding layer.  This is done to create a smoother depth gradient in the contour plot.  The \code{ctd_pts} data.frame must have three columns for longitude (\code{Long}), latitude (\code{Latitude}), and station name (\code{Station}).  The \code{dep_pts} data.frame should have three columns for longitude (\code{Long}), latitude (\code{Latitude}), and depth (\code{Depth}, non-negative).  The \code{plot} argument can be used to verify the match of the interpolated points with the depth soundings.
#'
#' The maximum depths and locations of the original stations are included in the search for the linearly expanded locations. This is done to create a seamless match between the observed depth and the hydrological depth soundings, i.e., locations at each station should have depths equal to observed and the interpolated points close to a station should have depth similar to the observed.
#'
#' All coordinates are assumed to be geographic decimal degrees using the WGS 1984 projection, negative longitude is west of the Prime Meridian.
#'
#' @return
#' The expanded data.frame from \code{ctd_pts} including depth (\code{Depth}) and cumulative distance (\code{Dist}, in km) columns.  If \code{plot = TRUE}, a ggmap plot is returned showing the matched points.
#'
#' @export
#'
#' @import dplyr geosphere ggmap ggplot2
#'
#' @examples
#' \dontrun{
#' get_depths(ctd_ex1, PB_dep_pts)
#' }
get_depths <- function(ctd_pts, dep_pts, expand = 200, plot = FALSE, show_bath = TRUE, zoom = 12){

  # get unique locations, max depth at each location
  ctd_pts <- select(ctd_pts, Station, Long, Lat, Depth) %>%
    group_by(Station) %>%
    filter(Depth == max(Depth)) %>%
    ungroup

  # expand value has to be changed to account for adding stations to the interpolated list
  expand <- expand - nrow(ctd_pts) + 2

  # expand ctd points by linear interp
  xint <- stats::approx(x = ctd_pts$Long, n = expand)$y
  yint <- stats::approx(x = ctd_pts$Lat, n = expand)$y
  locs <- data.frame(
  	Long = xint,
  	Lat = yint
  	)

  # get distance of new points from all depth points
  # find the depth points with the minimum distance to each point
  # depth of original stations are included in the search
  dep_chk <- select(ctd_pts, Depth, Long, Lat) %>%
    rbind(dep_pts)
  clo_dep <- distm(rbind(locs, dep_chk[, c('Long', 'Lat')])) %>%
    .[1:nrow(locs), ((1 + nrow(locs)):ncol(.))] %>%
    apply(1, function(x) which.min(x)[1])
  locs <- data.frame(locs, dep_chk[clo_dep, ])

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

  # for join segments on plot
  mtchs <- select(locs, Long, Lat, Long.1, Lat.1)

  # format output
  # add scale, smooth depth
  out <- mutate(locs,
    Station = NA
    ) %>%
    select(Station, Long, Lat, Depth, Dist) %>%
    .[-c(1, nrow(.)), ] %>%
    rbind(., ctd_pts) %>%
    arrange(Dist)

  # return plot if T
  if(plot){

    # extent
    ext <- make_bbox(out$Long, out$Lat)
    map <- get_stamenmap(ext, zoom = zoom, maptype = "toner-lite")

    # base map
    pbase <- ggmap(map) +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )

    # add depth points to interped
    if(show_bath){

      p <- pbase +
        geom_segment(data = mtchs, aes(x = Long, y = Lat, xend = Long.1, yend = Lat.1)) +
        geom_point(data = dep_chk, aes(x = Long, y = Lat), alpha = 0.8) +
        geom_point(data = out, aes(x = Long, y = Lat, fill = Depth), pch = 21, size = 3, colour = 'black', alpha = 0.8) +
        scale_fill_distiller(palette = 'Spectral')

    # otherwise just interped
    } else {

      p <- pbase +
        geom_point(data = out, aes(x = Long, y = Lat, fill = Depth), pch = 21, size = 3, colour = 'black', alpha = 0.8) +
        scale_fill_distiller(palette = 'Spectral')

    }

    return(p)

  }

  return(out)

}
