######
#' Example CTD data
#'
#' Example CTD data at nine stations on the longitudinal axis of the Escambia Bay sub-basin of Pensacola Bay, Florida.
#'
#' @format A \code{\link[base]{data.frame}} with 129 rows and 2 variables:
#' \describe{
#'   \item{\code{Station}}{chr, station name}
#'   \item{\code{Date}}{Date, Date of the survey}
#'   \item{\code{Depth}}{num, depth along the vertical profile (m)}
#'   \item{\code{Temp}}{num, water temperature}
#'   \item{\code{Salinity}}{num, salinity}
#'   \item{\code{SigmaT}}{num, density}
#'   \item{\code{DO}}{num, dissolved oxygen concentration}
#'   \item{\code{DOsat}}{num, saturation of dissolved oxygen}
#'   \item{\code{Fluor}}{num, fluorescence}
#'   \item{\code{Turb}}{num, turbidity}
#'   \item{\code{CDOM}}{num, colored dissolved organic matter}
#'   \item{\code{DateTimeStamp}}{POSIXct, date and time of the profile}
#'   \item{\code{dist}}{num, distance of the station along the axis}
#' }
#'
#' @examples
#' data(ctd_ex)
"ctd_ex"
