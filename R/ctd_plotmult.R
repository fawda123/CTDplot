######
#' Multiple contour plots for CTD data
#'
#' Multiple contour plots for CTD data with same color scale
#'
#' @param dat_in list of input ctd data where each element is a \code{\link[base]{data.frame}} of the vertical profiles for each station on a particular date
#' @param var_plo chr string of variable to plot from \code{dat_in}
#' @param dep_in depth soundings from bathymetric soundings, see \code{\link{get_depths}}
#' @param var_labs chr vector of optional text for each plot
#' @param ... additional arguments passed to \code{\link{ctd_plot}}
#'
#' @return The contour plots with identical color scaling
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- split(ctd, ctd$Date)
#'
#' # default plot
#' labs <- names(dat)
#' ctd_plotmult(dat, 'Salinity', PB_dep_pts, var_labs = labs)
#' }
ctd_plotmult <- function(dat_in, var_plo, dep_in = NULL, var_labs = NULL, ...){

  # range values
  rngs <- get_rngs(dat_in)

  par(mfrow = c(length(dat_in), 1))

  for(i in 1:length(dat_in))
    ctd_plot(dat_in[[i]], var_plo, dep_in, var_lab = var_labs[i], rngs_in = rngs, ...)

}
