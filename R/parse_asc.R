#' Take asc files and extract relevant data
#' Does not merge data with messages for fixation data, obtains sample report and puts time in ms
#' This code was written by Dr. Holger Mitterer
#' @param dirList list if asc files
#' @param homeDir directory of edf files
#' @param overwriteBlinks set to false
#' @param cutPreview
#' @export

parse_asc <- function(dirList, homeDir = "./", overwriteBlinks = FALSE, cutPreview = 0) {
  for (myDir in dirList) {
    myPP = myDir
    cat("\nworking on:", myDir, "\n")
    #extract numbers out of participant ID
    myID =regmatches(myDir,gregexpr('[A-Za-z]+',myDir)) %>% unlist()
    myDir = paste0(homeDir, myDir)
    hasFile = dir(myDir, pattern = ".asc")
    if (length(hasFile) != 1) {
      cat("Warning: No asc file found in directory: ", myDir, "\n")

    } else {
      #read in file ----
      myFile = paste0(myDir, "/", myPP, ".asc")
      myData = read.table(myFile, fill= T, header = F)
      messages = subset(myData, V1 == "MSG")

      #correct messages with a time offset ----

      #need a warningless function that checks whether it can be converted to a number
      check.numeric <- function(x)
      {
        x_num = suppressWarnings(as.numeric(x))
        x_isNum = !is.na(x_num)
      }

      # thirdIsNumber = check.numeric(messages$V3)
      # for (i in 1:nrow(messages))
      # {
      #   temp = messages[i,]
      #   if (thirdIsNumber[i])
      #   {
      #     messages[i,2] = as.numeric(temp$V2) - as.numeric(temp$V3)
      #     messages[i,3] = messages[i,4]
      #   }
      # }

      #generate an overview of the messages and trial variables present ----
      messageOverview = sort(table(messages$V3), decreasing = T)
      varDF = subset(messages, V4 == "TRIAL_VAR")
      varOverview = sort(table(varDF$V6))
      write.table(messageOverview, paste0(myDir, "/", "overview_", myID,".txt"), row.names = F)
      write.table(varOverview, paste0(myDir, "/", "Var_overview_", myID,".txt"), row.names = F)

      trialNumber = messageOverview["TRIALID"]

      #start making VWP data file ----
      #header is time	trial	ID	subject	pupil	x	y
      myOutFile = gsub(".asc","_vwp.csv",myFile)

      # create saccade file
      mySaccFile = gsub(".asc","_saccade.csv",myFile)
      
      # create fixation file
      myFixFile = gsub(".asc", "_fixation.csv",myFile)

      #initialize gaze variables
      time = NULL
      x = NULL
      y = NULL
      time = NULL
      pupil = NULL

      # saccade variables
      onset = NULL
      offset = NULL
      duration = NULL
      startx = NULL
      starty = NULL
      endx = NULL
      endy = NULL
      amplitude = NULL
      peak_velocity = NULL
      blink = NULL
      blinkbool = FALSE
      
      # fixation variables
      fixstart = NULL
      fixend = NULL
      fixduration = NULL
      avgx = NULL
      avgy = NULL
      avgpupil = NULL

      #is the first variable a number?
      is_data_point = grepl("^\\d+$",myData$V1)

      #get the start recording times of all trials
      start_recordings_DF = subset(messages, V3 == "!MODE")
      start_recordings = as.numeric(start_recordings_DF$V2)
      if (length(start_recordings) != trialNumber)
      {
        err_message = cat("Wrong number of start of recordings!!\n", trialNumber, " trials,
                      but ", length(start_recordings), "start of recordings\n")
        stop(err_message)
      }
      trial = 0
      #go through the whole file line byline
      for (i in 1:nrow(myData))
      {
        thisRow = myData[i, ]
        if (thisRow$V3 == "TRIALID")
        {
          #if TRIALID, start a new trial
          trial = trial + 1
          zero_time = start_recordings[trial]
          cat("\ntrial", trial, "by", myPP)
          #add header for first trial
          if (trial == 1){
            cat("time,trial,ID,subject,pupil,x,y\n", file = myOutFile)
            cat("trial,ID,subject,onset,offset,duration,startx,starty,endx,endy,amplitude,peak_velocity,blink\n", file = mySaccFile)
            cat("trial,ID,subject,onset,offset,duration,x,y,pupil\n", file = myFixFile)
          }
        }
        if (thisRow$V3 == "END") # changed from if (thisRow$V3 == "TRIAL_RESULT"); TRIAL_RESULT doesn't always exist
        {
          #wrap up trial if it is trial result
          x = suppressWarnings(as.numeric(x))
          y = suppressWarnings(as.numeric(y))
          x[is.na(x)] = 1e+08
          y[is.na(y)] = 1e+08
          #replace missing values by 10.000.000 as in the edf converted data
          pupil = as.numeric(pupil)
          #overwrite blinks option ----
          if (overwriteBlinks){
            nObs = length(x)
            #make sure trial starts with a valid data point
            if (x[1] > 10000){
              s_i = 2
              while ((x[s_i] > 10000) & (s_i < nObs))
              { s_i = s_i + 1 }
              for (s_j in s_i:1)
              {
                x[s_j+1] = x[s_j+1]
                y[s_j+1] = y[s_j+1]
                pupil[s_j+1] = pupil[s_j+1]
              }
            }
            #now correct forward
            for (s_i in 2:nObs)
            {
              if (x[s_i] > 10000)
              {
                x[s_i] = x[s_i-1]
                y[s_i] = y[s_i-1]
                pupil[s_i] = pupil[s_i-1]
              }
            }
            thisTrial  = data.frame(time, trial, ID = myID, subject = myPP, pupil, x, y)
          }else #end overwrite blinks option
          {
            thisTrial  = data.frame(time, trial, ID = myID, subject = myPP, pupil, x, y)
          }
          
          # create saccade data frame for current trial
          if (!is.null(onset)) { # check there were any saccades during the current trial
            thisTrialSaccade = data.frame(trial,
                                          ID = myID,
                                          subject = myPP,
                                          onset,
                                          offset,
                                          duration,
                                          startx,
                                          starty,
                                          endx,
                                          endy,
                                          amplitude,
                                          peak_velocity,
                                          blink)
          }

          # create fixation data frame for current trial
          if (!is.null(fixduration)) { # check if there were fixations for current trial (likely)
            thisTrialFixation = data.frame(trial,
                                          ID = myID,
                                          subject = myPP,
                                          fixstart,
                                          fixend,
                                          fixduration,
                                          avgx,
                                          avgy,
                                          avgpupil)
          }

          if (cutPreview > 0) { 
            thisTrial = subset(thisTrial, time > cutPreview) 
            if (!is.null(onset)) thisTrialSaccade = subset(thisTrialSaccade,onset > cutPreview) 
            if (!is.null(fixduration)) thisTrialFixation = subset(thisTrialFixation, onset > cutPreview)
          }
          fwrite(thisTrial, file= myOutFile, sep=",", append=TRUE , row.names=FALSE, col.names=FALSE)

          if (!is.null(onset)) fwrite(thisTrialSaccade, file = mySaccFile, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE) 
          if (!is.null(fixduration)) fwrite(thisTrialFixation, file = myFixFile, sep=",", append=TRUE, row.names=FALSE, col.names=FALSE)

          # reset gaze variables
          time = NULL
          x = NULL
          y = NULL
          time = NULL
          pupil = NULL

         # reset saccade variables
          onset = NULL
          offset = NULL
          duration = NULL
          startx = NULL
          starty = NULL
          endx = NULL
          endy = NULL
          amplitude = NULL
          peak_velocity = NULL
          blink = NULL
          blinkbool = FALSE
          
          # reset fixation variables
          fixstart = NULL
          fixend = NULL
          fixduration = NULL
          avgx = NULL
          avgy = NULL
          avgpupil = NULL

        }
        if (is_data_point[i])
        {
          time = c(time,as.numeric(thisRow$V1) - zero_time)
          x = c(x, thisRow$V2)
          y = c(y, thisRow$V3)
          pupil = c(pupil,thisRow$V4)
        }
        
        # check for blinks within saccade
        if (thisRow$V1 == 'EBLINK') blinkbool = TRUE
        
        # saccade (ADDED)
        if (thisRow$V1 == 'ESACC') {
          onset = c(onset,as.numeric(thisRow$V3) - zero_time)
          offset = c(offset, as.numeric(thisRow$V4) - zero_time)
          duration = c(duration,thisRow$V5)
          startx = c(startx,thisRow$V6)
          starty = c(starty, thisRow$V7)
          endx = c(endx, thisRow$V8)
          endy = c(endy, thisRow$V9)
          amplitude = c(amplitude, thisRow$V10)
          peak_velocity = c(peak_velocity,thisRow$V11)
          blink = c(blink,blinkbool)
          blinkbool = FALSE # set back to false for next saccade
        }
        
        if (thisRow$V1 == 'EFIX') {
          fixstart = c(fixstart,as.numeric(thisRow$V3) - zero_time)
          fixend = c(fixend, as.numeric(thisRow$V4) - zero_time)
          fixduration = c(fixduration,thisRow$V5)
          avgx = c(avgx,thisRow$V6)
          avgy = c(avgy,thisRow$V7)
          avgpupil = c(avgpupil,thisRow$V8)
        }
      }#end of loop through lines of asc file
    }
  }#end loop for directories
}
