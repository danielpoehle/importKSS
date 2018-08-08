setwd("/home/daniel/Dokumente/importKSS/")
source("ImportXMLKSS.R")

fNames <- c("mitte", "nord", "ost", "süd",  "südost", "südwest", "west")
folderNames <- paste0("./KSS2013_46/", fNames,"/")

for(i in 1:length(folderNames)){
    print(fNames[i])
    fileList <- list.files(folderNames[i], full.names = TRUE, pattern = ".xml$")
    tempTrainList <- list()
    for(j in 1:length(fileList)){
        print(j)
        tempTrainList <- c(tempTrainList, importXMLKSSFile(fileList[j]))  
    }
    for(k in 1:length(tempTrainList)){
        tempTrainList[[k]] <- setRegionalB(tempTrainList[[k]], fNames[i])
    }
    
    saveRDS(tempTrainList, file = paste0("./KSS2013_46/result/", fNames[i], ".rds"))
}







#########################################################################
### Deutschland-Gesamtliste
#########################################################################

folders <- list.files("./KSS2013_46/result", full.names = T, pattern = ".rds$")

completeDList <- list()

for(j in 1:length(folders)){
  fullList <- readRDS(file = folders[j])
  print(length(fullList))
  completeDList <- c(completeDList, fullList)
}

saveRDS(completeDList, file = paste0("./KSS2013_46/result/KSS2013_46-Gesamtdeutschland.rds"))

# load(file = "./KSS2014_43_imported/KSS2014_43-Gesamtdeutschland.RData")
completeDList <- readRDS(file = "./KSS2013_46/result/KSS2013_46-Gesamtdeutschland.rds")
# completeDList <- readRDS(file = "./KSS2013_46/result/mitte.rds")
source("ClassTrain.R")
len <- length(completeDList)

df <- data.frame(ID = integer(len), MAIN = integer(len), TFZ  = integer(len), 
                 NUM_TZF = integer(len), RB = integer(len), RUN20131114 = logical(len),
                 FIRST = integer(len), LAST = integer(len), TOTALLENGTH = integer(len),
                 WEIGHT = integer(len),PROD_MAIN = integer(len), 
                 PRODUCT = integer(len), TRAINCLASS = integer(len),
                 LZB = logical(len), BrH = integer(len), BREAKCLASS = integer(len),
                 VMAX = integer(len), VTSmain = integer(len), VTSholiday = integer(len),
                 VZEstart = integer(len), VZEend = integer(len), WAY = integer(len),
                 STOPS = integer(len), ARR = integer(len), DEP = integer(len))

for(i in 1:len){
    print(i)
    df$ID[i] <- getId(completeDList[[i]])
    df$MAIN[i] <- getIdMain(completeDList[[i]])
    df$TFZ[i] <- paste0(getTfzMain(completeDList[[i]]), "-", getTfzSub(completeDList[[i]]))
    df$NUM_TZF[i] <- getNumOfTfz(completeDList[[i]])
    df$RB[i] <- getRegionalB(completeDList[[i]])
    df$RUN20131114[i] <- departsOnDay(completeDList[[i]], 2013, 11, 14)
    st_list <- getAllStations(completeDList[[i]])
    df$FIRST[i] <- st_list$Ds100[1]
    df$LAST[i] <- st_list$Ds100[length(st_list$Ds100)]
    df$WAY[i] <- paste(st_list$Ds100, collapse = "#")
    df$ARR[i] <- paste(st_list$Arrival, collapse = "#")
    df$DEP[i] <- paste(st_list$Departure, collapse = "#")
    df$STOPS[i] <- paste(apply(st_list[!is.na(st_list$Type), c(1,4,5)], 1, paste, collapse = "?"), collapse = "#")
    df$TOTALLENGTH[i] <- getTotalLength(completeDList[[i]])
    df$WEIGHT[i] <- getTotalWeight(completeDList[[i]])
    df$PRODUCT[i] <- getProduct(completeDList[[i]])
    df$PROD_MAIN[i] <-  getProductMain(completeDList[[i]])
    df$TRAINCLASS[i] <- getTrainClass(completeDList[[i]])
    df$LZB[i] <- getLzb(completeDList[[i]])
    df$BrH[i] <- getBrH(completeDList[[i]])
    df$BREAKCLASS[i] <- getBreakingSystem(completeDList[[i]])
    df$VMAX[i] <- getMaxVelocity(completeDList[[i]])
    df$VTSmain[i] <- paste(getVTSMainNumber(completeDList[[i]]), collapse = "#")
    df$VTSholiday[i] <- getVtsHoliday(completeDList[[i]])
    df$VZEstart[i] <- getVZEBegin(completeDList[[i]])
    df$VZEend[i] <- getVZEEnd(completeDList[[i]])
}
write.csv2(df, file = "./KSS2013_46/KSS2013_46-Gesamtdeutschland_v04.csv", row.names = F)
