nnaData <- data
for(i in 1:17568){
    if(is.na(nnaData$steps[i])){
        inte <- nnaData$interval[i]
        val <- daily[daily$Interval==inte,"Average.Steps"]
        nnaData$steps[i] <- val
    }
}