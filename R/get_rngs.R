######
#' Get variable ranges
#'
#' Get the range of values for the same water quality variable between dates
#'
#' @param dat_in is list of data frames
#'
#' @return A \code{\link[base]{data.frame}} of the range of values for each variable
get_rngs <- function(dat_in){

  dat_in <- do.call('rbind', dat_in)

  out <- apply(dat_in[,c('Temp', 'Salinity', 'SigmaT', 'DO', 'DOsat',
      'Fluor', 'Turb', 'CDOM')],
    2, range)

  out <- data.frame(out)

  return(out)

  }
