#' Merge eye files from a ufolder
#' if filetype is from sr renames column variables and makes them lower case
#' Places time in ms
#' if edf files just reads them in
#'@import tidyverse
#'@import vroom
#' @param file_list path to .xls files
#' @param blink_colname name of your blink colname:AVERAGE_IN_BLINK, LEFT_, #RIGHT
#' @param pupil_colname name of your pupil colname:AVERAGE_IN_Pupil, LEFT_, #RIGHT
#' @param filetype if reading raw edf file use 'edf' if using sr files use type 'sr'
#' @export
#' @return data frame containing pupil data
merge_gazer_files <- function (file_list, blink_colname="blink", pupil_colname="pupil", filetype="sr") {  
  #file list is path to .xls files
  #vroom is faster
  library(data.table)

  if (filetype=="sr") {
    
  dataset <- do.call("rbind", lapply(file_list, FUN=function(files){
    
    for (i in 1:length(files)){ 
      if(file.exists(files[i])){
        message( "now processing:", files[i])
      }
    }
  
    fread(files, header=TRUE, sep="\t", na.strings = ".", fill=TRUE)})) #vroom makes reading in files quick
  
  change_name <- dataset %>% 
    dplyr::select(!!quo_name("subject"):= RECORDING_SESSION_LABEL,
         !!quo_name("blink"):= blink_colname,
         !!quo_name("pupil"):= pupil_colname,
         !!quo_name("trial"):= TRIAL_INDEX,
         !!quo_name("message"):= SAMPLE_MESSAGE,
         everything())
  
  names(change_name) <- tolower(names(change_name))
  
  change_name$time <- change_name$timestamp-change_name$ip_start_time

    return(as_tibble(change_name))
  }
  
if (filetype=="edf") { 
  
  dataset <- do.call("rbind", lapply(file_list, FUN=function(files){
    
    for (i in 1:length(files)){ 
      if(file.exists(files[i])){
        message( "now processing:", files[i])
      }
    }
    
    fread(files, header=TRUE, na.strings = "NA", fill=TRUE)})) #vroom makes reading in files quicke

   return(as_tibble(dataset))
  }

}
