######
#' Get variable ranges
#'
#' Get the range of values for the same water quality variable between dates
#'
#' @param dat_in is list of data frames
#'
#' @export
#'
#' @return A \code{\link[base]{data.frame}} of the range of values for each variable
get_rngs <- function(dat_in){

  dat_in <- do.call('rbind', dat_in)

  out <- apply(dat_in[, !names(dat_in) %in% c('Station', 'Date', 'Depth', 'dist', 'DateTimeStamp')],
    2, range)

  out <- data.frame(out)

  return(out)

  }
