#' Extract data from TRAFx shuttle files
#'
#' @description  The function \code{trafx_extract()} reads in semi-structured
#'   TRAFx data files, in \emph{.txt} format, and parses them into tabular data.
#'   The function relies heavily on \code{\link{grep}} pattern matching to parse
#'   out the header data for each download occurrence into separate colums.
#'   Header information is then carried forward for each time-stamped count
#'   records using \code{\link[zoo]{na.locf}}. \cr
#' 
#' @param path the name of the file which the data are to be read from or a
#'   directory (folder) of files for batch processing.
#' 
#' @param out the default output for \code{trafx_extract} is to return only the
#'   time-stamped count records \code{out = "data"}, which include
#'   \code{counter, counterID, dateTime, count1, count2}. Optionally you can
#'   return the header data from each download using \code{out = "head"} or a
#'   flat file containing all header and count columns using \code{out =
#'   "flat"}. Returning a flat dataset is generally discouraged as it makes for
#'   an unnecessarily large and complex output.
#' 
#' @details \code{trafx_extract} can take either a path to a single TRAFx
#'   shuttle file (which can contain multiple downloads) or to a directory
#'   contaning multiple shuttle files for batch processing. \cr
#' 
#'  NOTE: While \code{trafx_extract} will ignore any files that do not end in
#'  \emph{.txt}, it is strongly recomended to only include TRAFx shuttle files
#'  in the target directory if batch processing.
#' 
#' @export
#' @examples
#' # Extract data from a single shuttle file stored in a directory "data"
#' trafx_extract("data/ShuttleFile  190502-ALL_DB.TXT", out = "data")
#' 
#' # Batch process shuttle files from a directory "data" and extract
#' # header information for each download
#' trafx_extract("data", out = "head")
#' 

trafx_extract <- function(path, out = "data"){

# load required packages --------------------------------------------------

require(dplyr)
require(lubridate)
require(zoo)

# custom function to index strings from the right hand side
substrRight <- function(x,n){
  substr(x, nchar(x)-n+1, nchar(x))
}

if(file_test("-f", path)) {
  file.names <- path
} else if (file_test("-d", path)) {
  file.names <- list.files(path = path, pattern = ".txt", ignore.case = TRUE,
                           full.names = TRUE, recursive = TRUE)
} else {
  stop("The specified path is not a valid file or directory. If specifying
  a relative path, use getwd() to verify the current working directory.")
}

#-----------------------------------------------------------------------------------------------------------------
# lapply version of processing loop
#-----------------------------------------------------------------------------------------------------------------

tcParsed <- lapply(file.names, function(i){
  
  # Scan in shuttle file and convert to data frame
  tcOriginal <- data.frame(raw = scan(file = i,
                                      what = "raw", sep = "\n", blank.lines.skip = FALSE), stringsAsFactors = FALSE)
  
  # Parse hourly data records into seperate columns for each data part (keep metadata in first column)
  tcTransformed <- tcOriginal %>%
    mutate(rowID = seq_len(n()),
           type = ifelse(grepl("[[:digit:]]", substr(tcOriginal$raw,1,1)), "record","meta"), # categorize each row as metadata (header) or record (hourly counts)
           date = ifelse(type == "record", substr(raw,1,8),""),
           time = ifelse(type == "record", substr(raw,10,14),""),
           dateTime = as.POSIXct(ifelse(type == "record", substr(raw,1,14),""), "%y-%m-%d,%H:%M", tz = "MST"), # combine date & time to POSIXct
           count1 = ifelse(type == "record", substr(raw,16,20),""),
           count2 = ifelse(type == "record", substr(raw,22,26),""),
           serial = ifelse(substr(raw,1,16) == "  *Serial Number", substrRight(raw,6),NA),
           counter = ifelse(substr(raw,1,10) == "  *Counter", substr(raw,20,nchar(raw)),NA),
           counterID = gsub("[[:space:]]","",substr(counter,1,regexpr("-",counter, fixed = FALSE)-1)),
           mode = ifelse(substr(raw,1,7) == "  *Mode", substr(raw,20,nchar(raw)),NA),
           volt = ifelse(substr(raw,1,7) == "  *Batt", substr(raw,20,nchar(raw)),NA),
           downloadTime = as.POSIXct(ifelse(substr(raw,1,5) == "=TIME", substr(raw,24,nchar(raw)),NA), "%y-%m-%d,%H:%M", tz = "MST"),
           startTime = ifelse(substr(raw,1,6) == "=START", substr(raw,24,nchar(raw)),NA),
           dockTime = ifelse(substr(raw,1,5) == "=DOCK", substr(raw,29,nchar(raw)),NA)
    )
  
  tcTransformed$counter <- na.locf(tcTransformed$counter, na.rm = FALSE)
  tcTransformed$counterID <- na.locf(tcTransformed$counterID, na.rm = FALSE)
  tcTransformed$mode <- na.locf(tcTransformed$mode, na.rm = FALSE)
  tcTransformed$serial <- na.locf(tcTransformed$serial, na.rm = FALSE)
  tcTransformed$volt <- na.locf(tcTransformed$volt, na.rm = FALSE)
  tcTransformed$downloadTime <- na.locf(tcTransformed$downloadTime, na.rm = FALSE)
  tcTransformed$startTime <- na.locf(tcTransformed$startTime, na.rm = FALSE)
  tcTransformed$dockTime <- na.locf(tcTransformed$dockTime, na.rm = FALSE)
  
  # tc_data <- tcTransformed %>%
  #   filter(type == "record") %>%
  #   select(counter, counterID, dateTime, count1, count2)
  # 
  # tc_head <- tcTransformed %>%
  #   filter(type == "record") %>%
  #   distinct(counter, .keep_all = TRUE) %>%
  #   select(counter, counterID, mode, serial, volt, downloadTime, startTime,
  #          dockTime)
  
  return(tcTransformed %>%
    filter(type == "record") %>%
    select(counter, counterID, mode, serial, volt, downloadTime, startTime,
           dockTime, dateTime, count1, count2))
  
  # return(list(tc_data, tc_head, tc_flat))
})

tc_combined <- do.call(rbind, tcParsed)

if (out == "data"){
  
  output <- tc_combined %>%
    select(counter, counterID, dateTime, count1, count2)
  
} else if (out == "head"){
  
  output <- tc_combined %>%
    distinct(counter, .keep_all = TRUE) %>%
    select(counter, counterID, mode, serial, volt, downloadTime, startTime, dockTime)
  
} else if (out == "flat"){
  
  output <- tc_combined
}

return(output)

}