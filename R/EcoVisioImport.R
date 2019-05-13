#' Import EcoVisio CSV file
#'
#' \code{EcoVisioImport}Imports an EcoVisio 4 CSV export data file and
#' transforms into long format
#'
#' @param x the name of the file which the data are to be read from
#' @param type the count attributes included in the export file.
#' @export
#' @examples
#' EcoVisioImport()

# Returns:
#   A n*4 df of the kappa and P statistic for each pair of observers
  
  #-----------------------------------------------------------------------------
  # Check to see if required packages are installed and/or loaded for the
  # current user
  #-----------------------------------------------------------------------------

EcoVisioImport <- function(x, type){  
  #specify the packages of interest
  packages = c("dplyr")
  
  #use this function to check if each package is on the local machine
  #if a package is installed, it will be loaded
  #if any are not, the missing package(s) will be installed and loaded
  package.check <- lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
  
  #verify they are loaded
  search()
  

# Read in CSV file --------------------------------------------------------

  ev4 <- read.csv(x, stringsAsFactors = F)
  
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