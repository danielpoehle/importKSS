setwd("D:/RFiles/XMLKSS")

completeList <- read.csv2(file = "./KSS2013_46/KSS2013_46-Gesamtdeutschland_v03.csv", stringsAsFactors = F)
completeList <- completeList[completeList$RUN20131114,]
which(completeList$FIRST == completeList$LAST)
# filter all trains which have moer than 1 station
completeList <- completeList[completeList$FIRST != completeList$LAST, ]

sgv <- completeList$PROD_MAIN %in% c(seq(50,79,1), seq(-79,-50,1))
completeList <- completeList[sgv,]

trainNumbers <- unique(completeList$MAIN)

idList <- integer(length(trainNumbers))
doubleTrainNumber <- logical(length(trainNumbers))
additionalTrainNumber <- logical(length(trainNumbers))
loop <- logical(length(trainNumbers))

for(i in 1:length(trainNumbers)){
    print(i)
    tempFrame <- completeList[completeList$MAIN == trainNumbers[i],]
    if(length(tempFrame$ID) <= 1){
        #print(tempFrame$ID[1])
        idList[i] <- tempFrame$ID[1]
        next()
    }
    ds_first <- tempFrame$FIRST
    ds_first[grepl("\\s.$", tempFrame$FIRST)] <- gsub('.{1}$', '', tempFrame$FIRST[grepl("\\s.$", tempFrame$FIRST)])   
    ds_last <- tempFrame$LAST
    ds_last[grepl("\\s.$", tempFrame$LAST)] <- gsub('.{1}$', '', tempFrame$LAST[grepl("\\s.$", tempFrame$LAST)]) 
    first <- tempFrame[!(ds_first %in% ds_last) & ds_last %in% ds_first,]
    last <- tempFrame[!(ds_last %in% ds_first) & ds_first %in% ds_last,]
    if(length(first$ID) <1){
        #only a loop
        first <- tempFrame[which.max(ds_last %in% ds_first),]
    }
    if(any(length(tempFrame$ID) > 1 & !(ds_first %in% ds_last) & !(ds_last %in% ds_first))){
        s <- which(!(ds_first %in% ds_last) & !(ds_last %in% ds_first))
        if(length(s)>1){
            s <- s[s!=1]
        }
        for(x in s){
            f2 <- tempFrame[x,]
            if(first$ID[1] == f2$ID){
                stop(paste(i, "identical train IDs"))
                }
            #print(paste(i, f2))
            c2 <- f2$ID
            while(f2$LAST[1] %in% tempFrame$FIRST){
                f2 <- tempFrame[tempFrame$FIRST == f2$LAST[1],]
                if(grepl(f2$ID[1], c2)){break()}
                c2 <- paste(c2, f2$ID[1], sep = "#")
                }
            additionalTrainNumber[i] <- paste(additionalTrainNumber[i], c2, sep = "$")
            }
        }
    if(length(first$ID) >1){
            # more than one start
            
            # Rangierfahrt durch Bahnhof mit Tochterbetriebsstelle
            s <- which(grepl("\\s.$", first$FIRST))
            if(length(s)==1){
                if(gsub('.{1}$', '', first$FIRST[s]) %in% gsub('.{1}$', '', tempFrame$LAST)){
                    first <- first[-s,]
                }
            }else{
                if(length(first$ID) == 2){
                    if(first$FIRST[1] != first$FIRST[2] & first$LAST[1] != first$LAST[2]){
                        #both different starts and goals --> two different trains
                        # take the second tain and use the first one normal, see below: chain <- first$ID[1]
                        c2 <- first$ID[2]
                        f2 <- first[2,]
                        while(f2$LAST[1] %in% tempFrame$FIRST){
                            f2 <- tempFrame[tempFrame$FIRST == f2$LAST[1],]
                            if(grepl(f2$ID[1], c2)){break()}
                            c2 <- paste(c2, f2$ID[1], sep = "#")
                        }
                        doubleTrainNumber[i] <- c2
                    }else if(first$FIRST[1] == first$FIRST[2] & first$LAST[1] == first$LAST[2]){
                        #just different variants of the same train
                        doubleTrainNumber[i] <- FALSE
                    }else{
                        # different first or last station but one is equal
                        # check in detail!
                        f2 <- first[1,]
                        follower <- tempFrame[tempFrame$FIRST == f2$LAST[1],]
                        f_tfz <- which(follower$TFZ == first$TFZ)[1]
                        if(length(f_tfz) == 1){
                            first <- first[f_tfz,]
                        }else{
                            #no tfz is equal
                            #check in detail!
                            doubleTrainNumber[i] <- TRUE
                        }
                    }
                }else{
                    # 3 or more starts
                    # check in detail!
                    doubleTrainNumber[i] <- TRUE
                }
            }
        }
    chain <- first$ID[1] # take only the first train
    
    while(first$LAST[1] %in% tempFrame$FIRST){
        first <- tempFrame[tempFrame$FIRST == first$LAST[1],]
        if(grepl(first$ID[1], chain)){
            loop[i] <- T
            break()}
        chain <- paste(chain, first$ID[1], sep = "#")
    }
    idList[i] <- chain
}

addTrains <- unlist(strsplit(additionalTrainNumber[which(additionalTrainNumber!="FALSE")], "\\$"))
addTrains <- addTrains[which(addTrains != "FALSE")]
######################## check additional train numbers if they fit to master trains

for(i in 1:length(addTrains)){
    print(i)
    aTrain <- addTrains[i]
    main <- unlist(strsplit(aTrain, "-"))[1]
    long <- df$connectingIDs[df$TrainNumber == main]
    first <- completeList[completeList$ID ==unlist(strsplit(long, "#"))[1],]
    last <- completeList[completeList$ID ==unlist(strsplit(long, "#"))[length(unlist(strsplit(long, "#")))],]
    train <- completeList[completeList$ID ==aTrain,]
    if(gsub('.{1}$', '', first$FIRST) == gsub('.{1}$', '', train$LAST) & nchar(train$LAST) == 5){
        df$connectingIDs[df$TrainNumber == main] <- paste(aTrain, df$connectingIDs[df$TrainNumber == main], sep = "#")
        addTrains[i] <- "FALSE"
        print(paste("train", train$LAST, "first", first$FIRST))
    }
    
    if(gsub('.{1}$', '', last$LAST) == gsub('.{1}$', '', train$FIRST) & nchar(train$LAST) == 5){
        df$connectingIDs[df$TrainNumber == main] <- paste(df$connectingIDs[df$TrainNumber == main], aTrain, sep = "#")
        addTrains[i] <- "FALSE"
        print(paste("last", last$LAST, "train", train$FIRST))
    }
}

addTrains <- addTrains[which(addTrains != "FALSE")]
doubleTrains <-  doubleTrainNumber[doubleTrainNumber != "FALSE"]

df <- data.frame(TrainNumber = c(seq(1:length(addTrains)), 1000+seq(length(doubleTrains)), trainNumbers), connectingIDs = c(addTrains, doubleTrains, idList), stringsAsFactors = F)
write.csv2(df, file = "./KSS2013_46/Connections_v07.csv", row.names = F)

write.csv2(which(doubleTrainNumber == "TRUE"), file = "./KSS2013_46/DoubleTrainNumbers_v07.csv", row.names = F)
write.csv2(which(loop), file = "./KSS2013_46/Loops_v07.csv", row.names = F)






#################### check doubletten #########################


for(i in 1:length(df$TrainNumber)){
    if(i%%500 == 0){
        print(i)
    }
    numbers <- unlist(strsplit(df$connectingIDs[i], "#"))
    for(nu in numbers){
        for(j in i:length(df$TrainNumber)){
            if(i == j){
                if(sum(numbers == numbers) - length(numbers) > 0){
                    print(paste0("i: ", i, " doublette"))
                    }
                next()
                }
            checkNum <- unlist(strsplit(df$connectingIDs[j], "#"))
            if(nu %in% checkNum){print(paste0("i ", i, " j ", j, " doublette"))}
        }
    }
    }

