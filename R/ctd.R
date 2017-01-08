######
#' Example CTD data
#'
#' Example CTD data for seven sample dates at nine stations on the longitudinal axis of the Escambia Bay sub-basin of Pensacola Bay, Florida.
#'
#' @format A \code{\link[base]{data.frame}} with 173 rows and 13 variables:
#' \describe{
#'   \item{\code{Station}}{chr, station name}
#'   \item{\code{Date}}{Date, Date of the survey}
#'   \item{\code{Long}}{num, longitude of the station in geographic coordinates}
#'   \item{\code{Lat}}{num, latitude of the station in geographic coordinates}
#'   \item{\code{Depth}}{num, depth along the vertical profile (m)}
#'   \item{\code{Salinity}}{num, salinity}
#'   \item{\code{DO}}{num, dissolved oxygen concentration}
#' }
#'
#' @examples
#' data(ctd)
"ctd"
