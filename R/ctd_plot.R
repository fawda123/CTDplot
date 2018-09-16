######
#' Contour plot for CTD data
#'
#' Plot two-dimensional contours for CTD data by depth and distance
#'
#' @param dat_in ctd data along the tidal axis, as a \code{\link[base]{data.frame}}
#' @param var_plo chr string of variable to plot from \code{dat_in}
#' @param dep_in depth soundings from bathymetric soundings
#' @param date chr string of date to plot, only required if input data has more than one sample date
#' @param date_col chr string of name of date column
#' @param rngs_in output from \code{\link{get_rngs}}, used to scale the colors, see examples
#' @param num_levs numeric for number of contour levels
#' @param expand numeric for expanding sample size of CTD points, see \code{\link{get_depths}}
#' @param chop numeric for trimming the depth values
#' @param add numeric for scalar to add to all depth values
#' @param txt_scl numeric for scaling all text labels
#' @param stt_scl numeric for scaling station labels at the top independent of other labels
#' @param stt_txt logical if station labels on top are text for station names or triangles
#' @param plot logical if plot is returned, otherwise the interpolated data are returned
#' @param span numeric for smoothing factor to reduce jaggedness of depth values, passed to \code{\link[stats]{stats}}, set to 1e-6 to minimize the smooth
#' @param xlab chr string for x-axis label
#' @param ylab chr string for y-axis label
#' @param var_lab optional chr string of text to include in the plot
#' @param var_scl numeric for text size of \code{var_lab}
#' @param cols chr string of colors for the plot
#' @param msk_col chr string for color of masking depth
#' @param cont_ext numeric (meters) for reducing the mask size for the interpolated contour plot, this can be useful to remove white spaces that are not covered by the masking depth but may have an effect on the gradient in the color ramp
#' @param ylim numeric vector for y-axis limits, values must be negative for increasing depth
#' @param ncol numeric indicating degree of smoothing for the color palette
#'
#' @return The contour plot if \code{plot = TRUE}, otherwise a three-element list with the x values as distance in km, y values as depth, and z values as the interpolated CTD variable
#'
#' @import dplyr
#'
#' @export
#'
#' @details Raw data from the CTD are vertical profiles at unique stations.  The data are linearly interpolated along each profile and between stations to create a two-dimensional plotting surface. The \code{dat_in} data.frame must have at a minimum six columns for date (\code{Date}), longitude (\code{Long}), latitude (\code{Lat}), station name (\code{Station}), depth (\code{Depth}, non-negative), and the variable to plot.  The \code{dep_in} data.frame (if provided) should have three columns for longitude (\code{Long}), latitude (\code{Latitude}), and depth (\code{Depth}, non-negative).
#'
# All coordinates are assumed to be geographic decimal degrees using the WGS 1984 projection, negative longitude is west of the Prime Meridian.
#'
#' @references
#' Hagy III, JD, Murrell, MC. 2007. Susceptibility of a northern Gulf of Mexico estuary to hypoxia: An analysis using box models. Estuarine, Coastal, and Shelf Science. 74:239-253.
#'
#' @examples
#' # default plot
#' dt <- '2014-04-21'
#' ctd_plot(ctd, var_plo = 'Salinity', dep_in = PB_dep_pts, date = dt)
#'
#' # get interpolated data
#' ctd_plot(ctd, var_plo = 'Salinity', dep_in = PB_dep_pts, date = dt, plot = FALSE)
#'
#' # make color ramp match contour categories in legend
#' ctd_plot(ctd, 'Salinity', ncol = 8, date = dt)
#'
#' # change colors
#' ctd_plot(ctd, 'Salinity', cols = c('Blue', 'Purple', 'Orange'), date = dt)
ctd_plot <- function(dat_in, var_plo, dep_in = NULL, date = NULL, date_col = 'Date', rngs_in = NULL,
  num_levs = 8, expand = 200, span = 0.05, chop = 0, add = 0, txt_scl = 1, stt_scl = 0.7, stt_txt = TRUE, plot = TRUE,
  xlab = 'Channel distance (km)', ylab = 'Depth (m)', var_lab = NULL, var_scl = 1.5,
  cols = c('tomato', 'lightblue', 'lightgreen','green'), msk_col = 'grey',
  cont_ext = 0.5,
  ylim = NULL,
  ncol = 100){

  dat_in <- data.frame(dat_in, stringsAsFactors = FALSE)
  dat_in$Station <- as.character(dat_in$Station)

  # sanity check
  if(span <= 0)
    stop('window must be greater than zero')

  # stop if multiple dates and no date selection variable
  # otherwise select date
  uni_dts <- unique(dat_in[, date_col])
  if(length(uni_dts) > 1){
    if(is.null(date)){
      stop('Provide date if more than one sample date')
    } else {
      date <- as.Date(date)
      dat_in <- dat_in[dat_in[, date_col] %in% date, ]
    }
  }

  # get relevant data from input
  dat_in <- dat_in[, c('Station', 'Long', 'Lat', 'Depth', var_plo)]

  # extrapolate depths along the axis
  if(is.null(dep_in)){

    dep_pts <- get_bdepths(dat_in, expand = expand)

  } else {

    dep_pts <- get_depths(dat_in, dep_in, expand = expand)

  }

  # invert depth
  dep_pts <- mutate(dep_pts, Depth = -1 * Depth)

  # for plotting station location
  top <- filter(dep_pts, !is.na(Station)) %>%
    select(Station, Dist)

  # invert depth and add distance
  dat_in$Depth <- -1 * dat_in$Depth
  dat_in <- left_join(dat_in, top,  by = 'Station') %>%
    select(-Long, -Lat)

  # convert table long to wide
  dat_in <- reshape2::dcast(dat_in, Depth ~ Dist, value.var = var_plo)

  # complete depth to surface
  if(!0 %in% dat_in$Depth)
    dat_in <- rbind(dat_in, c(0, rep(NA, ncol(dat_in)-1)))

  # add rows for max depth of polygon box
  minint <- min(c(ylim[1], min(dep_pts$Depth)))
  minobs <- min(dat_in$Depth) - 0.25
  if(minint <= minobs){
    add_dep <- seq(minint, minobs, by = 0.25)
    add_dep <- c(add_dep, rep(NA, length = length(add_dep) * (ncol(dat_in)-1)))
    add_dep <-  matrix(add_dep, ncol = ncol(dat_in), byrow = F)
    add_dep <- as.data.frame(add_dep); names(add_dep) <- names(dat_in)
    dat_in <- rbind(add_dep, dat_in)
  }

  # fill leading NA with earliest obs value
  dat_in <- zoo::na.locf(dat_in, na.rm = FALSE)

  # flip by depth, fill trailing NA with last obs value
  dat_in <- dat_in[order(dat_in$Depth, decreasing = T), ]
  dat_in <- zoo::na.locf(dat_in)

  # x, y values for linear interp grid, long form
  new_grd <- expand.grid(
      approx(dat_in$Depth, n = expand)$y,
      approx(as.numeric(names(dat_in)[-1]), n = expand)$y
      )
  # get interped values
  int_val <- fields::interp.surface(
    obj = list(
      x = dat_in$Depth,
      y = as.numeric(names(dat_in)[-1]),
      z = dat_in[,-1]),
    loc = new_grd
    )

  # combine coords with interp values
  new_grd <- data.frame(new_grd, int_val)
  new_grd <- reshape2::dcast(new_grd, Var1 ~ Var2,
    value.var = 'int_val')

  # coords for plot
  y.val <- new_grd$Var1
  x.val <- as.numeric(names(new_grd)[-1])
  z.val <- as.matrix(new_grd[order(new_grd$Var1, decreasing = T),-1])

  # return data if plot is false
  if(!plot){
    out <- list(x = x.val, y = y.val, z = z.val)
    return(out)
  }

  # optional chop, smooth, and scalar for depth mask
  dep_pts <- dep_pts %>%
    mutate(
      Depth = pmin(-1 * chop, Depth),
      Depth = predict(loess(Depth ~ Dist, data = ., span = span, control = loess.control(surface = "direct"))),
      Depth = -1 * add + Depth
    )

  ##
  # start plot
  plot.new()

  # number of contours
  levs <- num_levs

  # mask z.val so correct col contours show up
  mask_grd <- sapply(1:expand,
    function(x){
      out <- rep(NA, expand)
      out[rev(y.val) >= (dep_pts$Depth[x] - cont_ext)] <- 1
      return(out)
    })
  z.val <- ifelse(mask_grd, z.val, NA)

  # add values for continuous colour ramp
  if(!is.null(rngs_in)){
    if(var_plo %in% names(rngs_in)){
      z.val[expand, 1] <- rngs_in[, var_plo][1]
      z.val[expand, 2] <- rngs_in[, var_plo][2]
      }
    }

  # plot margins
  par(new = "TRUE", plt = c(0.08,0.89,0.23,0.9), las = 1, cex.axis = 1 * txt_scl)

  # color function
  in_col <- colorRampPalette(cols)

  # default ylim
  if(is.null(ylim))
    ylim <- c(min(dep_pts$Depth), 0)

  # contour plot with isolines
  filled_contour(x = x.val, y = y.val, z = rotate(z.val),
    color.palette = in_col,
    ylab = '', xlab = '',
    nlevels = ncol, # for smoothed colors
    axes = F,
    xlim = c(0, max(dep_pts$Dist)),
    ylim = ylim)
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

  # x
  axis(side = 1, cex.axis = txt_scl)

  # y
  y.axs <- axTicks(2, par('yaxp'))
  axis(side = 2, at = y.axs, labels = abs(y.axs), cex.axis = txt_scl)

  # top station labels, as text or otherwise triangles
  if(stt_txt){
    axis(side = 3, at = top$Dist, labels = top$Station, cex.axis = stt_scl * txt_scl,
      tick = F, line = -0.75, xpd = T)
  } else {
    points(data.frame(x = top$Dist, y = 0), pch = 25, col = 'black', bg = 'black',
      xpd = T, adj = 0, cex = 1 * txt_scl)
  }

  # masking depth
  with(dep_pts,
    polygon(
      c(x.val, rev(x.val)),
      c(Depth, rep(min(c(y.val, Depth)), length(Depth))),
      col = msk_col
      ))

  ##
  # variable name -lower left
  if(!is.null(var_lab))
    text(x = par('usr')[1], 0.925 * par('usr')[3], labels = var_lab, pos = 4,
      cex = var_scl * txt_scl)

  box()

  ##
  # legend
  par(new = "TRUE", plt = c(0.91,0.95,0.23,0.9), las = 1, cex.axis = 1 * txt_scl)
  filled_legend(x.val, y.val, rotate(z.val), color.palette = in_col, xlab = "",
    nlevels = levs,
    ylab = "",
    ylim = c(min(z.val), max(z.val)))

}

#' Matrix rotation
#'
#' Rotate a matrix
#'
#' @param x input matrix
#'
#' @details Used internally in \code{\link{ctd_plot}}
rotate <- function(x) t(apply(x, 2, rev))
