library(stringr)
library(lubridate)

setClass(
    Class = "Train",
    representation=representation(
      id = "character",
      listOfStations = "data.frame",      
      tfzMain = "character",
      tfzSub = "character",
      numOfTfz = "numeric",
      totalLength = "numeric",
      totalWeight = "numeric",
      product = "character",
      productMain = "character",
      trainClass = "character",
      stopPosition = "character",
      lzb = "logical",
      brh = "numeric",
      tonnageRating = "logical",
      breakingSystem = "character",
      maxVelocity = "numeric",
      vtsMain = "character",
      vtsHoliday = "character",
      vzeBegin = "character",
      vzeEnd = "character",
      regionalB = "character"
      )
)

newT <- newTrain <- function(id,listOfStations, tfzMain, tfzSub, numOfTfz, totalLength, totalWeight, product, productMain, trainClass, stopPosition, lzb, brh, tonnageRating, breakingSystem, maxVelocity, vtsMain, vtsHoliday, vzeBegin, vzeEnd, regionalB){
    new (Class="Train",id=id, listOfStations= listOfStations, tfzMain=tfzMain, tfzSub=tfzSub, numOfTfz=numOfTfz, totalLength=totalLength, totalWeight=totalWeight, product=product, productMain=productMain, trainClass=trainClass, stopPosition=stopPosition, lzb=lzb, brh=brh, tonnageRating=tonnageRating, breakingSystem=breakingSystem, maxVelocity=maxVelocity, vtsMain=vtsMain, vtsHoliday=vtsHoliday, vzeBegin = vzeBegin, vzeEnd = vzeEnd, regionalB = regionalB)
}

decodeVts <- function(vts){
    if(is.na(as.integer(vts))){return(data.frame(Mo = T, Tu = T, We = T, Th = T, Fr = T, Sa = T, Su = T))}
    df <- data.frame(Mo = F, Tu = F, We = F, Th = F, Fr = F, Sa = F, Su = F)
    if(vts / 64 >=1){
        df$Mo <- TRUE
        vts <- vts%%64
    }
    if(vts / 32 >=1){
        df$Tu <- TRUE
        vts <- vts%%32
    }
    if(vts / 16 >=1){
        df$We <- TRUE
        vts <- vts%%16
    }
    if(vts / 8 >=1){
        df$Th <- TRUE
        vts <- vts%%8
    }
    if(vts / 4 >=1){
        df$Fr <- TRUE
        vts <- vts%%4
    }
    if(vts / 2 >=1){
        df$Sa <- TRUE
        vts <- vts%%2
    }
    if(vts >=1){
        df$Su <- TRUE
    }
    df
}

### Getter for "id"
setGeneric("getId",function(object){standardGeneric ("getId")})
setMethod("getId","Train",
            function(object){
              return(object@id)
              }
            )

### Getter for "id Main Number"
setGeneric("getIdMain",function(object){standardGeneric ("getIdMain")})
setMethod("getIdMain","Train",
          function(object){
            return(unlist(strsplit(object@id,"-"))[1])
          }
)

### Getter for "tfzMain"
setGeneric("getTfzMain",function(object){standardGeneric ("getTfzMain")})
setMethod("getTfzMain","Train",
          function(object){
            return(object@tfzMain)
          }
        )

### Getter for "tfzSub"
setGeneric("getTfzSub",function(object){standardGeneric ("getTfzSub")})
setMethod("getTfzSub","Train",
          function(object){
            return(object@tfzSub)
          }
         )

### Getter for "numOfTfz"
setGeneric("getNumOfTfz",function(object){standardGeneric ("getNumOfTfz")})
setMethod("getNumOfTfz","Train",
          function(object){
              return(object@numOfTfz)
          }
)

### Getter for "totalLength"
setGeneric("getTotalLength",function(object){standardGeneric ("getTotalLength")})
setMethod("getTotalLength","Train",
          function(object){
            return(object@totalLength)
          }
)

### Getter for "totalWeight"
setGeneric("getTotalWeight",function(object){standardGeneric ("getTotalWeight")})
setMethod("getTotalWeight","Train",
          function(object){
            return(object@totalWeight)
          }
)

### Getter for "product"
setGeneric("getProduct",function(object){standardGeneric ("getProduct")})
setMethod("getProduct","Train",
          function(object){
            return(object@product)
          }
)

### Getter for "productMain" --> Zuggattungshauptnummer
setGeneric("getProductMain",function(object){standardGeneric ("getProductMain")})
setMethod("getProductMain","Train",
          function(object){
              return(object@productMain)
          }
)

### Getter for "trainClass"
setGeneric("getTrainClass",function(object){standardGeneric ("getTrainClass")})
setMethod("getTrainClass","Train",
          function(object){
            return(object@trainClass)
          }
)

### Getter for "stopPosition"
setGeneric("getStopPosition",function(object){standardGeneric ("getStopPosition")})
setMethod("getStopPosition","Train",
          function(object){
            return(object@stopPosition)
          }
)

### Getter for "LZB"
setGeneric("getLzb",function(object){standardGeneric ("getLzb")})
setMethod("getLzb","Train",
          function(object){
            return(object@lzb)
          }
)

### Getter for "BrH"
setGeneric("getBrH",function(object){standardGeneric ("getBrH")})
setMethod("getBrH","Train",
          function(object){
            return(object@brh)
          }
)

### Getter for "tonnageRating"
setGeneric("getTonnageRating",function(object){standardGeneric ("getTonnageRating")})
setMethod("getTonnageRating","Train",
          function(object){
            return(object@tonnageRating)
          }
)

### Getter for "breakingSystem"
setGeneric("getBreakingSystem",function(object){standardGeneric ("getBreakingSystem")})
setMethod("getBreakingSystem","Train",
          function(object){
            return(object@breakingSystem)
          }
)

### Getter for "maxVelocity"
setGeneric("getMaxVelocity",function(object){standardGeneric ("getMaxVelocity")})
setMethod("getMaxVelocity","Train",
          function(object){
            return(object@maxVelocity)
          }
)

### Getter for "VTSMain"
setGeneric("getVtsMain",function(object){standardGeneric ("getVtsMain")})
setMethod("getVtsMain","Train",
          function(object){
            return(object@vtsMain)
          }
)

### Getter for "VTSHoliday"
setGeneric("getVtsHoliday",function(object){standardGeneric ("getVtsHoliday")})
setMethod("getVtsHoliday","Train",
          function(object){
            return(object@vtsHoliday)
          }
)

### Getter for "VZEBegin"
setGeneric("getVZEBegin",function(object){standardGeneric ("getVZEBegin")})
setMethod("getVZEBegin","Train",
          function(object){
            return(object@vzeBegin)
          }
)

### Getter for "VZEEnd"
setGeneric("getVZEEnd",function(object){standardGeneric ("getVZEEnd")})
setMethod("getVZEEnd","Train",
          function(object){
            return(object@vzeEnd)
          }
)

### Getter for "RegionalB"
setGeneric("getRegionalB",function(object){standardGeneric ("getRegionalB")})
setMethod("getRegionalB","Train",
          function(object){
            return(object@regionalB)
          }
)

### Setter for "RegionalB"
setGeneric("setRegionalB",function(object, rb){standardGeneric ("setRegionalB")})
setMethod("setRegionalB","Train",
          function(object, rb){
            object@regionalB <- rb
            return(object)
          }
)

### Getter for "ith Element of StationList"
setGeneric("getIthStation",function(object,i){standardGeneric ("getIthStation")})
setMethod("getIthStation","Train",
          function(object, i){
            if(i > length(object@listOfStations[,1])){
              stop("Index out of Range listOfStations")
            }else{
              return(object@listOfStations[i,])
            }            
          }
)

### Getter for "StationList"
setGeneric("getAllStations",function(object){standardGeneric ("getAllStations")})
setMethod("getAllStations","Train",
          function(object){
              return(object@listOfStations)            
          }
)

### Getter for "non NA StationList in DS100 and Departure"
setGeneric("getNonNAStationList",function(object){standardGeneric ("getNonNAStationList")})
setMethod("getNonNAStationList","Train",
          function(object){
            return(object@listOfStations[!is.na(object@listOfStations[,1]) & !is.na(object@listOfStations[,3]),])
                        
          }
)

### Getter for "DepartureTime for Station j"
setGeneric("getDepartureOfStation",function(object,j){standardGeneric ("getDepartureOfStation")})
setMethod("getDepartureOfStation","Train",
          function(object, j){
            st <- getNonNAStationList(object)
            row <- which(st[,1]==j)
            if(length(row) == 0){return (NA)}
            else{
              if(row == length(st[,1])){return (NA)}
              dep <- st[row,]
              return(dep[1,"Departure"])
            }            
          }
)

### Getter for "Departure on Day x"
setGeneric("departsOnDay",function(object,y,m,d){standardGeneric ("departsOnDay")})
setMethod("departsOnDay","Train",
          function(object,y,m,d){              
              actualday <- ymd(paste0(y, "-",m, "-",d))
              start <- ymd(as.character(getVZEBegin(object)))
              end <- ymd(as.character(getVZEEnd(object)))
              
              if(actualday < start | actualday > end) {return(FALSE)}
              
              vtsFrame <- decodeVts(as.numeric(getVtsMain(object)))
              weekd <- ifelse(wday(actualday)-1 == 0, 7, wday(actualday)-1)
              
              if(as.logical(vtsFrame[weekd])){return(TRUE)}
              return(FALSE)
          }
)

setMethod ("print","Train",
                 function(x,...){
                   if(length(x@id)!=0){
                     cat("**Train**************************** \n")
                     cat("* ID = "); print (getId(x))
                     cat("* StationList = "); print (str(x@listOfStations))
                     cat("* Anzahl Tfz = "); print (getNumOfTfz(x))
                     cat("* Erstes Tfz = "); print (getTfzMain(x))
                     cat("* Sub-Nr. = "); print (getTfzSub(x))
                     cat("* Ges. Länge = "); print (getTotalLength(x))
                     cat("* Ges. Gewicht = "); print (getTotalWeight(x))
                     cat("* Produkt = "); print (getProduct(x))
                     cat("* Zuggatungshauptnummer = "); print (getProductMain(x))
                     cat("* Zugklasse = "); print (getTrainClass(x))
                     cat("* Halteplatzart = "); print (getStopPosition(x))
                     cat("* Hat LZB = "); print (getLzb(x))
                     cat("* BrH = "); print (getBrH(x))
                     cat("* TonnageRating = "); print (getTonnageRating(x))
                     cat("* Bremssystem = "); print (getBreakingSystem(x))
                     cat("* vMax = "); print (getMaxVelocity(x))
                     cat("* VTSMain = "); print (getVtsMain(x))
                     cat("* VTSHoliday = "); print (getVtsHoliday(x))
                     cat("* VZEBegin = "); print (getVZEBegin(x))
                     cat("* VZEEnd = "); print (getVZEEnd(x))
                     cat("* Regionalbereich = "); print (getRegionalB(x))
                     cat("*********************************** \n")
                   }else{}
                 }
)

setMethod("show","Train",
                function(object){
                  if(length(object@id)!=0){
                    cat("**Train**************************** \n")
                    cat("* ID = "); print (getId(object))
                    cat("* StationList = "); print (str(object@listOfStations))
                    cat("* Anzahl Tfz = "); print (getNumOfTfz(object))
                    cat("* Erstes Tfz = "); print (getTfzMain(object))
                    cat("* Sub-Nr. = "); print (getTfzSub(object))
                    cat("* Ges. L?nge = "); print (getTotalLength(object))
                    cat("* Ges. Gewicht = "); print (getTotalWeight(object))
                    cat("* Produkt = "); print (getProduct(object))
                    cat("* Zuggatungshauptnummer = "); print (getProductMain(object))
                    cat("* Zugklasse = "); print (getTrainClass(object))
                    cat("* Halteplatzart = "); print (getStopPosition(object))
                    cat("* Hat LZB = "); print (getLzb(object))
                    cat("* BrH = "); print (getBrH(object))
                    cat("* TonnageRating = "); print (getTonnageRating(object))
                    cat("* Bremssystem = "); print (getBreakingSystem(object))
                    cat("* vMax = "); print (getMaxVelocity(object))
                    cat("* VTSMain = "); print (getVtsMain(object))
                    cat("* VTSHoliday = "); print (getVtsHoliday(object))
                    cat("* VZEBegin = "); print (getVZEBegin(object))
                    cat("* VZEEnd = "); print (getVZEEnd(object))
                    cat("* Regionalbereich = "); print (getRegionalB(object))
                    cat("*********************************** \n")
                  }else{}
                }
)

# load("tr.RDATA")
