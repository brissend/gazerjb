#' Performs linear (sub) or divisive baseline correction relative to desired stim_onset and off_set with messages. 
#' @param datafile raw pupil data
#' @param pupil_colname name of your pupil colname you want baseline corrected
#' @param baseline_dur duration of baseline period from stim offset
#' @param event_offset message name for stim onset
#' @param baseline_method correction method. Default is sub but can also include divisive
#' 
#' @return data frame containing baseline corrected data from event of interest
#' @export
baseline_correction_pupil_msg<-function(datafile, pupil_colname=NULL, baseline_dur=500, event_offset="event_offset", baseline_method="sub") 
{ 
  if (baseline_method=="sub") {
    message("Calculating median baseline")
    baseline <- datafile %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(event_offset_time=time[!is.na(message) & message==event_offset]) %>%
      dplyr::filter(time >= event_offset_time - baseline_dur,
                    time <= event_offset_time) %>%
                      dplyr::rename(pupil_avg = pupil_colname) %>%
                      dplyr::summarise(baseline = median(pupil_avg, na.rm=TRUE)) %>%
                      ungroup()
    
    message("Merging baseline")
    merge_baseline <- merge(baseline,datafile,all=TRUE) # merge median pupil size with raw dataset
    
    message("Performing subtractive baseline correction")
    
    corrected_baseline <- merge_baseline %>% 
      dplyr::rename(pupil_avg = pupil_colname) %>% 
      dplyr::mutate(baselinecorrectedp = pupil_avg - baseline) %>%
      dplyr::rename(pup_interp = pupil_avg) %>%
      dplyr::arrange(subject, trial, time)
  }
  
  if (baseline_method=="div") { 
    message("Calculating median baseline from",":", event_offset)
    baseline <- datafile %>%
      dplyr::group_by(subject, trial) %>%
      dplyr::mutate(event_offset_time=time[!is.na(message) & message==event_offset]) %>%
      dplyr::filter(time >= event_offset_time - baseline_dur,
                    time <= event_offset_time) %>%
      dplyr::rename(pupil_avg = pupil_colname) %>%
      dplyr::summarise(baseline = median(pupil_avg, na.rm=TRUE)) %>%
      ungroup()
    
    message("Merging baseline")
    merge_baseline <- merge(baseline,datafile,all=TRUE) # merge median pupil size with raw dataset
    
    message("Performing divisive baseline correction")
    
    corrected_baseline <- merge_baseline %>% 
      dplyr::rename(pupil_avg = pupil_colname) %>% 
      dplyr::mutate(baselinecorrectedp = (pupil_avg - baseline)/baseline) %>%
      dplyr::rename(pup_interp = pupil_avg) %>%
      dplyr::arrange(subject, trial, time)
    
  }
  
  return(corrected_baseline)
}