setwd("/home/daniel/Dokumente/importKSS/")

completeList <- read.csv2(file = "./KSS2013_46/KSS2013_46-Gesamtdeutschland_v05.csv", stringsAsFactors = F)
completeList <- completeList[completeList$RUN20131114,]
#which(completeList$FIRST == completeList$LAST)

circles <- completeList[completeList$FIRST == completeList$LAST, ]
# filter all trains which have moer than 1 station
completeList <- completeList[completeList$FIRST != completeList$LAST, ]

# filter only sgv
sgv <- completeList$PROD_MAIN %in% c(seq(50,79,1), seq(-79,-76,1), seq(-74,-50,1), -36)
completeList <- completeList[sgv,]

# filter out studien
l1 <- strsplit(completeList$ID, "-")
stud <- logical(length(l1))
for(i in 1:length(l1)){
  if(as.numeric(l1[[i]][2]) >= 900){
    stud[i] <- T
  }
}

completeList <- completeList[!stud,]

write.csv2(completeList, file = "./KSS2013_46/CompleteSGVList_14-11_v11.csv", row.names = F)

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
    first_id <- which(!(ds_first %in% ds_last) & ds_last %in% ds_first)
    first <- tempFrame[first_id,]
    last <- tempFrame[!(ds_last %in% ds_first) & ds_first %in% ds_last,]
    if(length(first$ID) <1){
        #only a loop, compare departure time
        times <- strsplit(tempFrame$DEP, "#")
        t_list <- numeric(0)
        for(j in 1:length(times)){
          ti <- unlist(strsplit(times[[j]][1], ":"))
          t_list[j] <- as.numeric(ti[1])*3600 + as.numeric(ti[2])*60 + as.numeric(unlist(strsplit(ti[3], "\\."))[1])
        }
        first_id <- which.min(t_list)
        first <- tempFrame[first_id,]
    }
    if(any(length(tempFrame$ID) > 1 & !(ds_first %in% ds_last) & !(ds_last %in% ds_first))){
        s <- which(!(ds_first %in% ds_last) & !(ds_last %in% ds_first))
        len_s <- length(s)
        if(length(first$ID)< 1){
          first <- tempFrame[s[1],]
        }
        if(len_s>1){
            s <- s[s!=first_id]
        }
        for(x in s){
            f2 <- tempFrame[x,]
            if(len_s > 1 && first$ID[1] == f2$ID){
                stop(paste(i, "identical train IDs"))
                }
            #print(paste(i, f2))
            c2 <- f2$ID
            f2_id <- x
            while(ds_last[f2_id] %in% ds_first){
              f2_id <- which(ds_first == ds_last[f2_id])
              if(length(f2_id)>1){
                # if two sucessors take the one with different destinaton to first
                # if both sucessors have different destinations take the first one
                f2_id <- f2_id[(ds_last[f2_id] != ds_first[f2_id][1])][1]
              }
              f2 <- tempFrame[f2_id,]
              if(grepl(f2$ID[1], c2)){
                break()
              }
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
                        f2_id <- first_id[2]
                        f2 <- tempFrame[f2_id,]

                        while(ds_last[f2_id] %in% ds_first){
                          f2_id <- which(ds_first == ds_last[f2_id])
                          if(length(f2_id)>1){
                            # if two sucessors take the one with different destinaton to first
                            # if both sucessors have different destinations take the first one
                            f2_id <- f2_id[(ds_last[f2_id] != ds_first[f2_id][1])][1]
                          }
                          f2 <- tempFrame[f2_id,]
                          if(grepl(f2$ID[1], c2)){
                            break()
                          }
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
    # take only the first train
    first_id <- first_id[1]
    chain <- first$ID[1]


    while(ds_last[first_id] %in% ds_first){
        first_id <- which(ds_first == ds_last[first_id])
        if(length(first_id)>1){
          # if two sucessors take the one with different destinaton to first
          first_id <- first_id[(ds_last[first_id] != ds_first[first_id][1])]
          if(length(first_id) > 1){
            # if both sucessors have different destinations remove a dead end
            # if no dead end is there take the first sucessor
            if(length(first_id[ds_last[first_id] %in% ds_first])>0){
              first_id <- first_id[ds_last[first_id] %in% ds_first][1]
            }else{
              first_id <- first_id[1]
            }
          }
        }
        first <- tempFrame[first_id,]
        if(grepl(first$ID[1], chain)){
            loop[i] <- T
            break()
            }
        chain <- paste(chain, first$ID[1], sep = "#")
    }
    idList[i] <- chain
}

addTrains <- unlist(strsplit(additionalTrainNumber[which(additionalTrainNumber!="FALSE")], "\\$"))
addTrains <- addTrains[which(addTrains != "FALSE")]
doubleTrains <-  doubleTrainNumber[doubleTrainNumber != "FALSE"]

##### check if all train numbers are still
allIDs <- completeList$ID

tmp <- (unlist(strsplit(idList, "#")))
tmp <- c(addTrains, (unlist(strsplit(doubleTrains, "#"))), tmp)

missing <- integer(0)
m_id <- integer(0)
for(i in 1:length(allIDs)){
  if(!(allIDs[i] %in% tmp)){
    missing <- c(missing, allIDs[i])
    m_id <- c(m_id, i)
  }
}

missing[7]
idList[which(trainNumbers == 95283)]
additionalTrainNumber[which(trainNumbers == 95283)]
completeList[completeList$MAIN == "95283",1:21]

write.csv2(missing, file = "./KSS2013_46/Missing_v11.csv", row.names = F)

######################## check additional train numbers if they fit to master trains

# for(i in 1:length(addTrains)){
#     print(i)
#     aTrain <- addTrains[i]
#     main <- unlist(strsplit(aTrain, "-"))[1]
#     long <- df$connectingIDs[df$TrainNumber == main]
#     first <- completeList[completeList$ID ==unlist(strsplit(long, "#"))[1],]
#     last <- completeList[completeList$ID ==unlist(strsplit(long, "#"))[length(unlist(strsplit(long, "#")))],]
#     train <- completeList[completeList$ID ==aTrain,]
#     if(gsub('.{1}$', '', first$FIRST) == gsub('.{1}$', '', train$LAST) & nchar(train$LAST) == 5){
#         df$connectingIDs[df$TrainNumber == main] <- paste(aTrain, df$connectingIDs[df$TrainNumber == main], sep = "#")
#         addTrains[i] <- "FALSE"
#         print(paste("train", train$LAST, "first", first$FIRST))
#     }
#
#     if(gsub('.{1}$', '', last$LAST) == gsub('.{1}$', '', train$FIRST) & nchar(train$LAST) == 5){
#         df$connectingIDs[df$TrainNumber == main] <- paste(df$connectingIDs[df$TrainNumber == main], aTrain, sep = "#")
#         addTrains[i] <- "FALSE"
#         print(paste("last", last$LAST, "train", train$FIRST))
#     }
# }
#
# addTrains <- addTrains[which(addTrains != "FALSE")]


df <- data.frame(TrainNumber = c(seq(1:length(addTrains)), 1000+seq(length(doubleTrains)), trainNumbers), connectingIDs = c(addTrains, doubleTrains, idList), stringsAsFactors = F)
write.csv2(df, file = "./KSS2013_46/Connections_v11.csv", row.names = F)

write.csv2(which(doubleTrainNumber == "TRUE"), file = "./KSS2013_46/DoubleTrainNumbers_v10.csv", row.names = F)
write.csv2(which(loop), file = "./KSS2013_46/Loops_v10.csv", row.names = F)

write.csv2(circles, file = "./KSS2013_46/Single_v11.csv", row.names = F)

#################### check if ascending order of departure times ################


dep <- completeList[, c("ID", "DEP")]
na_list <- ""
for(i in 1:length(dep$ID)){
  print(i)
  dep$FIRST[i] <- unlist(strsplit(dep$DEP[i], "#"))[1]

  ti <- unlist(strsplit(dep$FIRST[i], ":"))
  hours <- as.numeric(ti[1])
  if(is.na(hours)){
    na_list <- paste(na_list, i, sep = "#")
    dep$SECONDS[i] <- -1
    next()
  }
  hours <- ifelse(hours >= 100, 24 + hours - 100, hours)
  minutes <- as.numeric(ti[2])
  dep$SECONDS[i] <- hours*3600 + minutes*60 + as.numeric(unlist(strsplit(ti[3], "\\."))[1])
}


for(i in 1:length(df$TrainNumber)){
  conn_list <- unlist(strsplit(df$connectingIDs[i], "#"))
  startTimes <- integer(0)
  for(c in conn_list){
    startTimes <- c(startTimes, dep$SECONDS[c == completeList$ID])
  }
  if(!is.unsorted(startTimes)){
    df$SORTED[i] <- T
  }else{
    df$SORTED[i] <- F
  }
  df$TIMES[i] <- paste(startTimes, collapse = "#")
}

write.csv2(df, file = "./KSS2013_46/TimeSort_Connections_v11.csv", row.names = F)

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

