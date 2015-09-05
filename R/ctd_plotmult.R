######
#' Multiple contour plots for CTD data
#'
#' Multiple contour plots for CTD data with same color scale
#'
#' @param dat_in list of input ctd data where each element is a \code{\link[base]{data.frame}} of the vertical profiles for each station on a particular date
#' @param ... additional arguments passed to \code{\link{ctd_plot}}
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

  # range values
  rngs <- get_rngs(dat_in)

  par(mfrow = c(length(dat_in), 1))

  for(dt in dat_in) ctd_plot(dt, rngs_in = rngs, ...)

}
