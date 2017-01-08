######
#' Plot contours of bottom water variable by station and time
#'
#' Plot two-dimensional CTD contours for bottom water variable by station and time
#'
#' @param dat_in ctd data along the tidal axis, as a \code{\link[base]{data.frame}}
#' @param var_plo chr string of variable to plot from \code{dat_in}
#' @param date_col chr string of name of date column
#' @param num_levs numeric for number of contour levels
#' @param expand numeric for expanding sample size of CTD points, see \code{\link{get_depths}}
#' @param txt_scl numeric for scaling all text labels
#' @param xlab chr string for x-axis label
#' @param ylab chr string for y-axis label
#' @param cols chr string of colors for the plot
#' @param ncol numeric indicating degree of smoothing for the color palette
#'
#' @return The contour plot
#'
#' @import dplyr
#'
#' @export
#'
#' @details Raw data from the CTD are vertical profiles at unique stations for more than one date.  The data are linearly interpolated between dates and stations to create a two-dimensional plotting surface. The \code{dat_in} data.frame must have at a minimum six columns for date (\code{Date}), longitude (\code{Long}), latitude (\code{Lat}), station name (\code{Station}), depth (\code{Depth}, non-negative), and the variable to plot.
#'
# All coordinates are assumed to be geographic decimal degrees using the WGS 1984 projection, negative longitude is west of the Prime Meridian.
#'
#' Sample dates are shown as triangles on the top and station locations are shown on the right.
#'
#' @examples
#' # default plot
#' ctd_bplot(ctd, var_plo = 'DO')
ctd_bplot <- function(dat_in, var_plo, date_col = 'Date',
  num_levs = 8, expand = 200, txt_scl = 1,
  xlab = 'Date', ylab = 'Channel distance (km)',
  cols = c('tomato', 'lightblue', 'lightgreen','green'),
  ncol = 100){

  # stop if only one date
  uni_dts <- unique(dat_in[, date_col])
  if(length(uni_dts) == 1){
    stop('More than one date is required to plot')
  }

  # get distance between stations
  dists <- dat_in$Date %in% uni_dts[1] %>%
    dat_in[., ] %>%
    get_bdepths(expand = nrow(.)) %>%
    select(-Depth, -Long, -Lat)

  # get relevant data from input
  # column select, then variable on bottom
  # join with distances
  dat_in <- dat_in[, c('Station', date_col, 'Depth', var_plo)]
  names(dat_in)[names(dat_in) %in% date_col] <- 'Date'
  names(dat_in)[names(dat_in) %in% var_plo] <- 'var_plo'
  dat_in <- group_by(dat_in, Date, Station) %>%
    filter(Depth == max(Depth)) %>%
    ungroup %>%
    select(-Depth) %>%
    left_join(., dists, by = 'Station')

  # convert long to wide
  dat_in <- reshape2::dcast(dat_in, Dist ~ Date, value.var = 'var_plo')

  # expand dates by day
  dts <- range(uni_dts)
  dts <- seq(dts[1], dts[2], by = 'day')

  # x, y values for linear interp grid, long form
  new_grd <- expand.grid(
    approx(dists$Dist, n = expand)$y,
    dts
    )
  # get interped values
  int_val <- fields::interp.surface(
    obj = list(
      x = dat_in$Dist,
      y = as.numeric(uni_dts),
      z = as.matrix(dat_in[, -1])
      ),
    loc = new_grd
    )

  # combine coords with interp values
  new_grd <- data.frame(new_grd, int_val)
  new_grd <- reshape2::dcast(new_grd, Var1 ~ Var2,
    value.var = 'int_val')

  # coords for plot
  y.val <- new_grd$Var1
  x.val <- as.numeric(dts)
  z.val <- as.matrix(new_grd[order(new_grd$Var1, decreasing = T),-1])

  ##
  # start plot
  plot.new()

  # number of contours
  levs <- num_levs

  # plot margins
  par(new = "TRUE", plt = c(0.08,0.87,0.23,0.9), las = 1, cex.axis = 1 * txt_scl)

  # color function
  in_col <- colorRampPalette(cols)

  # contour plot with isolines
  filled_contour(x = x.val, y = y.val, z = rotate(z.val),
    color.palette = in_col,
    ylab = '', xlab = '',
    nlevels = ncol, # for smoothed colors
    axes = F)
  contour(x = x.val, y = y.val, z = rotate(z.val), nlevels=levs,
    axes = F, add = T, labcex = 0.6 * txt_scl)

  ##
  # axis labels

  # xlab
  mtext(text = xlab, side = 1, line = 3, cex = txt_scl)

  # ylab
  mtext(text = ylab, side = 2, line = 3, las = 0, cex = txt_scl)

  ##
  # axes

  # bottom
  axis.Date(side = 1, x = dts, format = '%Y-%m-%d', cex.axis = txt_scl)

  # left
  y.axs <- axTicks(2, par('yaxp'))
  axis(side = 2, at = y.axs, labels = abs(y.axs), cex.axis = txt_scl)

  # top
  points(data.frame(x = uni_dts, y = max(dists$Dist)), pch = 25, col = 'black', bg = 'black',
    xpd = T, adj = 0, cex = 1 * txt_scl)

  # right
  axis(side = 4, at = dists$Dist, labels = dists$Station, cex.axis = 0.7 * txt_scl,
    tick = F, line = -0.75)

  box()

  ##
  # legend
  par(new = "TRUE", plt = c(0.91,0.95,0.23,0.9), las = 1, cex.axis = 1 * txt_scl)
  filled_legend(x.val, y.val, rotate(z.val), color.palette = in_col, xlab = "",
    nlevels = levs,
    ylab = "",
    ylim = c(min(z.val), max(z.val)))

}
