
source("ClassTrain.R")
library("XML")
library(stringr)

importXMLKSSFile <- function(fileName){
  trainList <- list()
  
  # Load XML into memory
  text <- readLines(fileName, encoding="ISO-8859-1")
  if(any(grep(paste0("[", rawToChar(as.raw(c(1:8,10:31))), "]"), text))){
    print("Non-printable characters in file!")
    print(fileName)
    
    cat("Lines "); print (grep(paste0("[", rawToChar(as.raw(c(1:8,10:31))), "]"), text))
    return
  }
  root <- xmlRoot(xmlTreeParse(text, asTree = TRUE, options = HUGE))
  
  # get number of TrainPaths
  maxInd <- length(root[[3]][[1]])
  
  # Iteration über alle Trassen i
  for(i in 2:maxInd){
    # cat("* Trasse i = "); print (i)
    constrTrainList <- root[[3]][[1]][[i]][[2]][names(root[[3]][[1]][[i]][[2]])=="constructionTrain"]
    connectionList <- root[[3]][[1]][[i]][[2]][names(root[[3]][[1]][[i]][[2]])=="connection"]
    
    # Iteration ?ber alle Konstruktionsz?ge j der Trasse i
    for(j in 1:length(constrTrainList)){
      
      # cat("* Zug j = "); print (j)
      # nur Z?ge, die nicht Phantomz?ge sind aufnehmen
      phantom <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["isPhantom"]])
      if(!is.na(phantom) & phantom == "false"){
        id <- paste(xmlValue(constrTrainList[j]$constructionTrain[["trainNumber"]][["mainNumber"]]), 
                    xmlValue(constrTrainList[j]$constructionTrain[["trainNumber"]][["subNumber"]]), 
                    xmlValue(constrTrainList[j]$constructionTrain[["trainNumber"]][["userAbbreviation"]]), sep = "-")
        
        # Aufbau der stationList
        # Iteration ?ber alle Stationen k des Konstruktionszuges j in Trasse i
        listStations <- data.frame()
        #cat("* Zug = "); print (i)
        if(is.null(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]])){next}
        for(k in 1:length(xmlChildren(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]]))){
          ds100 <- xmlValue(xmlChildren(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]])[k]$sequenceServicePoint[["servicePoint"]])
          arrival <- xmlValue(xmlChildren(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]])[k]$sequenceServicePoint[["arrival"]])
          departure <- xmlValue(xmlChildren(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]])[k]$sequenceServicePoint[["departure"]])
          type <- xmlValue(xmlChildren(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]])[k]$sequenceServicePoint[["stopMode"]])
          #print(type)
          if(!is.na(type)){
              type <- paste0(xmlAttrs(xmlChildren(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]])[k]$sequenceServicePoint[["stopMode"]])[["Typ"]],": ", type)
          }else{
              type <- NA
          }
          minTime <- xmlValue(xmlChildren(constrTrainList[j]$constructionTrain[["sequence"]][["sequenceServicePoints"]])[k]$sequenceServicePoint[["minStopTime"]])
          if(is.na(minTime)){
              minTime <- ""
          }
          
          listStations <- rbind(listStations, data.frame(Ds100=ds100, Arrival=arrival, Departure=departure, Type = type, MinStopTime <- minTime, stringsAsFactors = FALSE))
        }
        
        # Tfz Haupt- und Unternummer
        numOfTfz <- length(constrTrainList[j]$constructionTrain[["characteristic"]][["tractionUnits"]])
        tfzMain <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["tractionUnits"]][[1]][["tractionUnitDesignSeries"]][["designSeries"]])
        tfzSub <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["tractionUnits"]][[1]][["tractionUnitDesignSeries"]][["variante"]])
        
        # Gesamtl?nge des Zuges
        totLength <- as.numeric(xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["totalLength"]]))
        
        # Gesamtmasse des Zuges
        totWeight <- as.numeric(xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["totalWeight"]]))
        
        # Zuggattung des Zuges
        prod <- xmlAttrs(constrTrainList[j]$constructionTrain[["characteristic"]][["kind"]])[["ZuggattungsProdukt"]]
        
        # Zuggattungshauptnummer des Zuges
        prodMain <- xmlAttrs(constrTrainList[j]$constructionTrain[["characteristic"]][["kind"]])[["Hauptnummer"]]
        
        # Zugklasse des Zuges
        trClass <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["trainClass"]])
        
        # Halteplatzart des Zuges
        stopPos <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["relevantStopPositionMode"]])
        
        # LZB ja nein
        hasLZB <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["trainProtection"]]) == "true"
        
        # Bremshundertstel
        brh <- as.numeric(xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["brakedWeightPercentage"]]))
        
        # H?chstgeschwindigkeit
        vMax <- as.numeric(xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["maxVelocity"]]))
        
        # Tonnage Rating
        tonRating <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["tonnageRating"]]) == "true"
        
        # Breaking System
        breaking <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["brakingSystem"]])
        
        # VTS der Trasse und Verkehrszeitraum
        timeframes <- constrTrainList[j]$constructionTrain[["services"]]
        desc <-xmlAttrs(timeframes[[1]])["description"]
        VTS_main <- "#"
        VZE_begin <- "#"
        VZE_end <- "#"
        AddDays <- ""
        ExclDays <- ""
        
        for(m in 1:length(timeframes)){
          beg <- xmlAttrs(timeframes[[m]])["startDate"]
          if(!is.na(beg)){
            ende <- xmlAttrs(timeframes[[m]])["endDate"]
            for(v in 1:length(timeframes[[m]])){
              if(!is.na(xmlAttrs(timeframes[[m]][[v]])["dayType"]) && xmlAttrs(timeframes[[m]][[v]])["dayType"] == "regularday"){
                vt <- xmlAttrs(timeframes[[m]][[v]])["operatingCode"]
              }
              if(!is.na(xmlAttrs(timeframes[[m]][[v]])["type"]) && xmlAttrs(timeframes[[m]][[v]])["type"] == "include"){
                if(AddDays == ""){
                  AddDays <- xmlAttrs(timeframes[[m]][[v]])["date"]
                }else{
                  AddDays <- paste(AddDays, xmlAttrs(timeframes[[m]][[v]])["date"], sep = "#")
                }
              }
              if(!is.na(xmlAttrs(timeframes[[m]][[v]])["type"]) && xmlAttrs(timeframes[[m]][[v]])["type"] == "exclude"){
                if(ExclDays == ""){
                  ExclDays <- xmlAttrs(timeframes[[m]][[v]])["date"]
                }else{
                  ExclDays <- paste(ExclDays, xmlAttrs(timeframes[[m]][[v]])["date"], sep = "#")
                }
              }
            }
            if(VTS_main == "#"){
              VTS_main <- vt
            }else{
              VTS_main <- paste(VTS_main, vt, sep = "#")
            }
            
            if(VZE_begin == "#"){
              VZE_begin <- beg
            }else{
              VZE_begin <- paste(VZE_begin, beg, sep = "#")
            }
            
            if(VZE_end == "#"){
              VZE_end <- ende
            }else{
              VZE_end <- paste(VZE_end, ende, sep = "#")
            }
          }else{
            for(v in 1:length(timeframes[[m]])){
              if(!is.na(xmlAttrs(timeframes[[m]][[v]])["type"]) && xmlAttrs(timeframes[[m]][[v]])["type"] == "include"){
                if(AddDays == ""){
                  AddDays <- xmlAttrs(timeframes[[m]][[v]])["date"]
                }else{
                  AddDays <- paste(AddDays, xmlAttrs(timeframes[[m]][[v]])["date"], sep = "#")
                }
              }
              if(!is.na(xmlAttrs(timeframes[[m]][[v]])["type"]) && xmlAttrs(timeframes[[m]][[v]])["type"] == "exclude"){
                if(ExclDays == ""){
                  ExclDays <- xmlAttrs(timeframes[[m]][[v]])["date"]
                }else{
                  ExclDays <- paste(ExclDays, xmlAttrs(timeframes[[m]][[v]])["date"], sep = "#")
                }
              }
            }
          }
          
           
        }
        
        VTS_holiday <- str_sub(desc[[1]], start = nchar(desc)-2, end = nchar(desc)-1)
        if(grepl("\\.", VTS_holiday)){
          VTS_holiday <- "00"
        }
        
        # Liste der Konstruktionsz?ge erg?nzen
        # cat("* ID = "); print (id)
        # cat("* Tfz Main = "); print (tfzMain)
        trainList <- c(trainList, newTrain(id, listStations, tfzMain, tfzSub, numOfTfz, 
                                           totLength, totWeight, prod, prodMain, trClass, 
                                           stopPos, hasLZB, brh, tonRating, breaking, vMax, 
                                           VTS_main, VTS_holiday, VZE_begin, VZE_end, 
                                           AddDays, ExclDays, ""))
        # function(id,listOfStations, tfzMain, tfzSub, totalLength, totalWeight, product, trainClass, stopPosition, lzb, brh, tonnageRating, breakingSystem, maxVelocity)
      }
    }
  }
  
  rm(root)
  trainList
}

importXMLKSSFromDirectory <- function(directoryName){
  completeTrainList <- list()
  
  # Erzeugung des Pfads in der Form "./directory"
  pathString <- paste("./", directoryName, sep = "")
  
  # Holen aller Dateien im Ordner
  fileList <- list.files(pathString, full.names = TRUE, pattern = ".xml$")
  
  if(length(fileList) == 0){
    return(completeTrainList)
  }
  
  # Einlesen der einzelnen Dateien
  for(i in 1:length(fileList)){
    completeTrainList <- c(completeTrainList, importXMLKSSFile(fileList[i]))
  }
  
  completeTrainList
}