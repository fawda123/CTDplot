#' Match CTD points with closest hydro soundings
#'
#' Match CTD points with closest hydro soundings
#'
#' @param ctd_pts data.frame of CTD station locations, see details
#' @param dep_pts data.frame of depth sounding points, see details
#' @param expand numeric for expansion factor for CTD points
#' @param add numeric for scalar to add to all depth values
#' @param window numeric for smoothing factor to reduce jaggedness of depth values, default does nothing
#' @param plot logical to return plot of matched points
#' @param zoom numeric for map zoom, passed to \code{\link[ggmap]{get_stamenmap}}
#'
#' @details
#' CTD station locations are linearly expanded and then matched to the nearests points in the depth sounding layer.  This is done to create a smoother depth gradient in the contour plot.  The \code{ctd_pts} data.frame must have three columns for longitude (\code{Long}), latitude (\code{Latitude}), and station name (\code{Station}).  The \code{dep_pts} data.frame should have three columns for longitude (\code{Long}), latitude (\code{Latitude}), and depth (\code{Depth}, non-negative).  The \code{plot} argument can be used to verify the match of the interpolated points with the depth soundings.
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
#' data(PB_pts)
#' data(PB_dep_pts)
#'
#' get_depths(PB_pts, PB_dep_pts)
get_depths <- function(ctd_pts, dep_pts, expand = 10, add = 0, window = 1, plot = FALSE, zoom = 12){

  # expand ctd points by linear interp
  xint <- stats::approx(x = ctd_pts$Long, n = expand * nrow(ctd_pts))$y
  yint <- stats::approx(x = ctd_pts$Lat, n = expand * nrow(ctd_pts))$y
  locs <- data.frame(
  	Long = xint,
  	Lat = yint
  	)

  # get distance of new points from all depht points
  # find the depth points with the minimum distance to each point
  clo_dep <- distm(rbind(locs, dep_pts[, c('Long', 'Lat')])) %>%
    .[1:nrow(locs), ((1 + nrow(locs)):ncol(.))] %>%
    apply(1, function(x) which.min(x)[1])
  locs <- data.frame(locs, dep_pts[clo_dep, ])

  # estimate cumulative distance between points from geosphere (km)
  # extract top part of lower triangle
  dist <- distm(locs[, c('Long', 'Lat')])/1000
  d <- row(dist) - col(dist)
  dist <- split(dist, d)$`1`
  locs$dist <- c(0, cumsum(dist))

  # format output
  # add scale, smooth depth
  out <- rename(locs,
      Dist = dist
      ) %>%
    mutate(
      Depth = add + pmax(0, Depth),
      Depth = as.numeric(stats::filter(Depth, sides = 1, filter = rep(1, window)/window)),
      Depth = rev(zoo::na.locf(rev(Depth)))
    )
  row.names(out) <- 1:nrow(out)

  # return plot if T
  if(plot){

    # extent
    ext <- make_bbox(out$Long, out$Lat)
    map <- get_stamenmap(ext, zoom = zoom, maptype = "toner-lite")

    # map
    p <- ggmap(map) +
      geom_segment(data = out, aes(x = Long, y = Lat, xend = Long.1, yend = Lat.1)) +
      geom_point(data = dep_pts, aes(x = Long, y = Lat), alpha = 0.8) +
      geom_point(data = out, aes(x = Long, y = Lat, fill = Depth), pch = 21, size = 3, colour = 'black', alpha = 0.8) +
      scale_fill_distiller(palette = 'Spectral') +
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      )
    return(p)

  }

  # remove extra lat/long
  out <- select(out, -Long.1, -Lat.1)

  return(out)

}
