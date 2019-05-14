#' Import EcoVisio CSV file
#'
#' \code{ecovisio_import} imports a CSV data file exported from EcoVisio 4 and
#' transforms the data into long format
#'
#' @param path the name of the file which the data are to be read from
#' @export
#' @examples
#' EcoVisioImport()

#-----------------------------------------------------------------------------
# Check to see if required packages are installed and/or loaded for the
# current user
#-----------------------------------------------------------------------------

ecovisio_import <- function(path){  
  require(dplyr)
  
# Read in CSV file --------------------------------------------------------

  ev4 <- read.csv(path, stringsAsFactors = F)
  
  ev4$Date <- as.POSIXct(ev4$Date, format = "%m/%d/%Y %H:%M")
  
  y <- seq(2, (ncol(ev4)-2), by = 3)
  
  d1 <- lapply(y, function(i){
    d <- ev4 %>%
      select(1, i:(i+2) ) %>%
      mutate(location = gsub("[.]", " ", colnames(.)[2])) %>%
      rename(Total = colnames(.)[2],
             In = colnames(.)[3],
             Out = colnames(.)[4]) %>%
      select(c(5,1,2:4))
  })
  
  d2 <- do.call(rbind, d1)
  
  return(d2)
  
}