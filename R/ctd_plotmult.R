######
#' Multiple contour plots for CTD data
#'
#' Multiple contour plots for CTD data with same color scale
#'
#' @param dat_in list of input ctd data where each element is a \code{\link[base]{data.frame}} of the vertical profiles for each station on a particular date
#' @param ... additional arguments passed to \code{\link{ctd_dat}}
#'
#' @return The contour plots with identical color scaling
#'
#' @export
#'
#' @examples
#' dat <- list(ctd_ex1, ctd_ex2)
#'
#' # default plot
#' ctd_plotmult(dat, 'Salinity')
ctd_plotmult <- function(dat_in, ...){

  # maximum depth of data for each station, used for polygon masking
  # value is linearly interpolated to increase samp
  maxd <- depth_tran

  # size of intepolation grid on one axis
  num_int <- 200

  # get relevant data from input, convert units for some
  dat_in <- dat_in[, c('Station', 'Depth', var_plo, 'dist')]

  dat_in$Depth <- -1 * dat_in$Depth
  dat_in$dist <- dat_in$dist * 1.6093

  # for plotting station location
  top <- unique(dat_in[, c('Station', 'dist')])

  # convert table long to short
  dat_in <- reshape2::dcast(dat_in, Depth ~ dist, value.var = var_plo)

  # complete depth to surface
  if(!0 %in% dat_in$Depth)
    dat_in <- rbind(dat_in, c(0, rep(NA, ncol(dat_in)-1)))

  # add rows for max depth of polygon box
  add_dep <- seq(min(maxd$mllw_m), min(dat_in$Depth) - 0.25, by = 0.25)
  add_dep <- c(add_dep, rep(NA, length = length(add_dep) * (ncol(dat_in)-1)))
  add_dep <-  matrix(add_dep, ncol = ncol(dat_in), byrow = F)
  add_dep <- as.data.frame(add_dep); names(add_dep) <- names(dat_in)
  dat_in <- rbind(add_dep, dat_in)

  # fill leading NA with earliest obs value
  dat_in <- zoo::na.locf(dat_in)

  # flip by depth, fill trailing NA with last obs value
  dat_in <- dat_in[order(dat_in$Depth, decreasing = T), ]
  dat_in <- zoo::na.locf(dat_in)

  # x, y values for linear interp grid, long form
  new_grd <- expand.grid(
      approx(dat_in$Depth, n = num_int)$y,
      approx(as.numeric(names(dat_in)[-1]), n = num_int)$y
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

  ##
  # start plot
  plot.new()

  # number of contours
  levs <- num_levs

  # mask z.val so correct col contours show up
  mask_grd <- sapply(1:num_int,
    function(x){
      dep_chk <- approx(maxd$mllw_m, n = num_int)$y
      out <- rep(NA, num_int)
      out[rev(y.val) >= (dep_chk[x])- 0.75] <- 1
      return(out)
    }
  )
  z.val <- ifelse(mask_grd, z.val, NA)

  # add values for continuous colour ramp
  if(!is.null(rngs_in)){
    if(var_plo %in% names(rngs_in)){
      z.val[200, 1] <- rngs_in[, var_plo][1]
      z.val[200, 2] <- rngs_in[, var_plo][2]
      }
    }

  # plot margins
  par(new = "TRUE", plt = c(0.15,0.85,0.3,0.9), las = 1, cex.axis = 1)

  # color function
  in_col <- colorRampPalette(cols)

  # contour plot with isolines
  filled_contour(x = x.val, y = y.val, z = rotate(z.val),
    color.palette = in_col,
    ylab = ylab, xlab='',
    nlevels = ncol, # for smoothed colors
    axes = F)
  contour(x = x.val, y = y.val, z = rotate(z.val), nlevels=levs,
    axes = F, add = T)

  ##
  # axis labels

  # xlab
  mtext(text = xlab, side = 1, line = 3)

  ##
  # axes

  # x
  axis(side = 1)

  # y
  y.axs <- axTicks(2, par('yaxp'))
  axis(side = 2, at = y.axs, labels = abs(y.axs))

  # top
  axis(side = 3, at = top$dist, labels = top$Station, cex.axis = 0.7,
    tick = F, line = -1)

  # masking depth
  poly.x <- approx(x.val, n = nrow(maxd))$y
  with(maxd,
    polygon(
      c(poly.x, rev(poly.x)),
      c(mllw_m, rep(min(mllw_m), length(mllw_m))),
      col = 'grey'
      ))

  ##
  # variable name -lower left
  if(var_lab)
    text(x = par('usr')[1], par('usr')[3] + 1, labels = var_plo, pos = 4,
      cex = 1.5)

  box()

  ##
  # legend
  par(new = "TRUE", plt = c(0.87,0.91,0.3,0.9), las = 1, cex.axis = 1)
  filled_legend(x.val, y.val, rotate(z.val), color.palette = in_col, xlab = "",
    nlevels = levs,
    ylab = "",
    ylim = c(min(z.val), max(z.val)))
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
