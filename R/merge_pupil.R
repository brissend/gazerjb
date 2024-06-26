#' Merge pupil files from a folder
#' Renames column variables and makes them lower case
#' Places time in ms
#'@import tidyverse
#'@import data.table
#' @param file_list path to .xls files
#' @param blink_colname name of your blink colname:AVERAGE_IN_BLINK, LEFT_, #RIGHT
#' @param pupil_colname name of your pupil colname:AVERAGE_IN_Pupil, LEFT_, #RIGHT
#' @param filetype if reading raw edf file use 'edf' if using sr files use type 'sr'
#' @export
#' @return data frame containing pupil data
merge_pupil <- function (file_list, blink_colname="", pupil_colname="", filetype="sr") {  
  #file list is path to .xls files
  library(data.table)

  if (filetype=="sr") {
    
  dataset <- do.call("rbind", lapply(file_list, FUN=function(files){
    
    for (i in 1:length(files)){ 
      if(file.exists(files[i])){
        message( "now processing:", files[i])
      }
    }
  
    
    fread(files, header=TRUE, sep="\t", na.strings = ".", fill=TRUE)})) #fread makes reading in files quicke
  
    change_name <- select(dataset,subject=RECORDING_SESSION_LABEL, trial =  TRIAL_INDEX,    blink = blink_colname, pupil = pupil_colname, everything())
  
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
    
    fread(files, header=TRUE, na.strings = "NA", fill=TRUE)})) #fread makes reading in files quicke
 
   return(as_tibble(dataset))
  }

}
