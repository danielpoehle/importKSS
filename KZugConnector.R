setwd("/home/daniel/Dokumente/importKSS/")

connections <- read.csv2(file = "./KSS2013_46/Connections_v12_manuell.csv", stringsAsFactors = F)
trains <- read.csv2(file = "./KSS2013_46/CompleteSGVList_14-11_v12.csv", stringsAsFactors = F)

result = data.frame(ANFORDERUNGID  = integer(length(connections$TrainNumber)),  ANFORDERUNGNAME  = integer(length(connections$TrainNumber)),  FIRST_BTS  = integer(length(connections$TrainNumber)),  LAST_BTS  = integer(length(connections$TrainNumber)),  STARTZEIT  = integer(length(connections$TrainNumber)),  ZIELZEIT  = integer(length(connections$TrainNumber)),  TFZ  = integer(length(connections$TrainNumber)),  NUM_TFZ  = integer(length(connections$TrainNumber)),  TOTALLENGTH  = integer(length(connections$TrainNumber)),  TOTALWEIGHT  = integer(length(connections$TrainNumber)),  ZUGGATTUNGSHAUPTNR  = integer(length(connections$TrainNumber)),  PRODUCT  = integer(length(connections$TrainNumber)),  TRAINCLASS  = integer(length(connections$TrainNumber)),  LZB  = logical(length(connections$TrainNumber)),  BrH  = integer(length(connections$TrainNumber)),  BREAKCLASS  = integer(length(connections$TrainNumber)),  VMAX  = integer(length(connections$TrainNumber)),  VTSMAIN  = integer(length(connections$TrainNumber)),  VTSHOLIDAY  = integer(length(connections$TrainNumber)),  VZE_START  = integer(length(connections$TrainNumber)),  VZE_END  = integer(length(connections$TrainNumber)),  CHARARCTERISTIC_CHANGE  = integer(length(connections$TrainNumber)),  WAY  = integer(length(connections$TrainNumber)),  STOPS  = integer(length(connections$TrainNumber)),  ARR  = integer(length(connections$TrainNumber)),  DEP = integer(length(connections$TrainNumber)), stringsAsFactors = F)


for(i in 1:length(connections$TrainNumber)){
  print(i)
  parts <- unlist(strsplit(connections$connectingIDs[i], "#"))
  result$ANFORDERUNGID[i] <- connections$TrainNumber[i]
  result$ANFORDERUNGNAME[i] <- unlist(strsplit(parts[1], "-"))[1]
  first_part <- trains[trains$ID == parts[1], ]
  if(length(first_part$ID) != 1){stop("first part not unique")}

  last_part <- trains[trains$ID == parts[length(parts)], ]
  if(length(last_part$ID) != 1){stop("last part not unique")}

  result$FIRST_BTS[i] <- first_part$FIRST
  result$LAST_BTS[i] <- last_part$LAST
  tempTime <- unlist(strsplit(first_part$DEP, "#"))
  result$STARTZEIT[i] <- tempTime[tempTime != "NA"][1]
  tempTime <- unlist(strsplit(last_part$ARR, "#"))
  if(sum(tempTime == "NA") == length(tempTime == "NA")){
    tempTime <- unlist(strsplit(last_part$DEP, "#"))
    if(sum(tempTime == "NA") == length(tempTime == "NA")){
      tempTime <- -1
    }
  }
  result$ZIELZEIT[i] <- tempTime[tempTime != "NA"][sum(tempTime != "NA")]
  result$TFZ[i] <- first_part$TFZ
  result$NUM_TFZ[i] <- first_part$NUM_TZF
  result$TOTALLENGTH[i] <- first_part$TOTALLENGTH
  result$TOTALWEIGHT[i] <- first_part$WEIGHT
  result$ZUGGATTUNGSHAUPTNR[i] <- first_part$PROD_MAIN
  result$PRODUCT[i] <- first_part$PRODUCT
  result$TRAINCLASS[i] <- first_part$TRAINCLASS
  result$LZB[i] <- first_part$LZB
  result$BrH[i] <- first_part$BrH
  result$BREAKCLASS[i] <- first_part$BREAKCLASS
  result$VMAX[i] <- first_part$VMAX
  result$VTSMAIN[i] <- first_part$VTSmain
  result$VTSHOLIDAY[i] <- first_part$VTSholiday
  result$VZE_START[i] <- first_part$VZEstart
  result$VZE_END[i] <- first_part$VZEend
  if(length(parts) == 1){
    result$CHARARCTERISTIC_CHANGE[i] <- ""
    result$WAY[i] <- first_part$WAY
    result$STOPS[i] <- first_part$STOPS
    result$ARR[i] <- first_part$ARR
    result$DEP[i] <- first_part$DEP
  }else{
    change <- ""
    wy <- first_part$WAY
    st <- first_part$STOPS
    ar <- first_part$ARR
    dp <- first_part$DEP
    for(j in 2:length(parts)){
      tmp_change <- ""
      old_part <- trains[trains$ID == parts[j-1], ]
      new_part <- trains[trains$ID == parts[j], ]
      if(length(new_part$ID) != 1){stop("new part not unique")}

      if(old_part$TFZ != new_part$TFZ){
        txt <- paste0("TFZ ", new_part$TFZ)
        tmp_change <- ifelse(tmp_change == "", txt, paste(tmp_change, txt, sep = "$"))
      }
      if(old_part$NUM_TZF != new_part$NUM_TZF){
        txt <- paste0("NUM_TZF ", new_part$NUM_TZF)
        tmp_change <- ifelse(tmp_change == "", txt, paste(tmp_change, txt, sep = "$"))
      }
      if(old_part$TOTALLENGTH != new_part$TOTALLENGTH){
        txt <- paste0("TOTALLENGTH ", new_part$TOTALLENGTH)
        tmp_change <- ifelse(tmp_change == "", txt, paste(tmp_change, txt, sep = "$"))
      }
      if(old_part$WEIGHT != new_part$WEIGHT){
        txt <- paste0("WEIGHT ", new_part$WEIGHT)
        tmp_change <- ifelse(tmp_change == "", txt, paste(tmp_change, txt, sep = "$"))
      }
      if(old_part$LZB != new_part$LZB){
        txt <- paste0("LZB ", new_part$LZB)
        tmp_change <- ifelse(tmp_change == "", txt, paste(tmp_change, txt, sep = "$"))
      }
      if(old_part$BrH != new_part$BrH){
        txt <- paste0("BrH ", new_part$BrH)
        tmp_change <- ifelse(tmp_change == "", txt, paste(tmp_change, txt, sep = "$"))
      }
      if(old_part$BREAKCLASS != new_part$BREAKCLASS){
        txt <- paste0("BREAKCLASS ", new_part$BREAKCLASS)
        tmp_change <- ifelse(tmp_change == "", txt, paste(tmp_change, txt, sep = "$"))
      }
      if(tmp_change != ""){
        txt <- paste0(new_part$FIRST, ":", tmp_change)
        change <- ifelse(change == "", txt, paste(change, txt, sep = "#"))
      }
      wy <- paste(wy, new_part$WAY, sep = "#")
      st <- paste(st, new_part$STOPS, sep = "#")
      ar <- paste(ar, new_part$ARR, sep = "#")
      dp <- paste(dp, new_part$DEP, sep = "#")
    }
    result$CHARARCTERISTIC_CHANGE[i] <- change
    result$WAY[i] <- wy
    result$STOPS[i] <- st
    result$ARR[i] <- ar
    result$DEP[i] <- dp
  }
}

write.csv2(result, file = "./KSS2013_46/Fahrlagen_14.11.2013_final_v12.csv", row.names = F)
