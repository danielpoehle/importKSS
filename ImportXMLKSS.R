
source("ClassTrain.R")
library("XML")
library(stringr)

importXMLKSSFile <- function(fileName){
  trainList <- list()
  
  # Load XML into memory
  text <- readLines(fileName)
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
    
    # Iteration über alle Konstruktionszüge j der Trasse i
    for(j in 1:length(constrTrainList)){
      
      # cat("* Zug j = "); print (j)
      # nur Züge, die nicht Phantomzüge sind aufnehmen
      phantom <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["isPhantom"]])
      if(!is.na(phantom) & phantom == "false"){
        id <- paste(xmlValue(constrTrainList[j]$constructionTrain[["trainNumber"]][["mainNumber"]]), 
                    xmlValue(constrTrainList[j]$constructionTrain[["trainNumber"]][["subNumber"]]), 
                    xmlValue(constrTrainList[j]$constructionTrain[["trainNumber"]][["userAbbreviation"]]), sep = "-")
        
        # Aufbau der stationList
        # Iteration über alle Stationen k des Konstruktionszuges j in Trasse i
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
        
        # Gesamtlänge des Zuges
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
        
        # Höchstgeschwindigkeit
        vMax <- as.numeric(xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["maxVelocity"]]))
        
        # Tonnage Rating
        tonRating <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["tonnageRating"]]) == "true"
        
        # Breaking System
        breaking <- xmlValue(constrTrainList[j]$constructionTrain[["characteristic"]][["brakingSystem"]])
        
        # VTS der Trasse und Verkehrszeitraum
        desc <-xmlAttrs(constrTrainList[j]$constructionTrain[["services"]][["service"]])["description"]
        VTS_main <- str_sub(desc[[1]], start = nchar(desc)-6, end = nchar(desc)-4)
        if(grepl("\\.", VTS_main)){
            VTS_main <- desc
        }
        VTS_holiday <- str_sub(desc[[1]], start = nchar(desc)-2, end = nchar(desc)-1)
        VZE_begin <- xmlAttrs(constrTrainList[j]$constructionTrain[["services"]][["service"]])["startDate"][1]
        VZE_end <- xmlAttrs(constrTrainList[j]$constructionTrain[["services"]][["service"]])["endDate"][1]
        
        # Liste der Konstruktionszüge ergänzen
        # cat("* ID = "); print (id)
        # cat("* Tfz Main = "); print (tfzMain)
        trainList <- c(trainList, newTrain(id, listStations, tfzMain, tfzSub, numOfTfz, totLength, totWeight, prod, prodMain, trClass, stopPos, hasLZB, brh, tonRating, breaking, vMax, VTS_main, VTS_holiday, VZE_begin, VZE_end, ""))
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