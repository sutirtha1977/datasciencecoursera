pollutantmean <- function(directory,pollutant,id=1:332){
        idIndex <- id
        rawdataset <- combine(idIndex,directory)
        cleandataset <- rawdataset[!is.na(rawdataset[c(pollutant)]),c(pollutant)]
        return (mean(cleandataset))
}

combine <- function(idIndex, directory){
        startI <- 1
        endI <- length(idIndex)
        binded <- data.frame("mean"=integer(0))
        
        while(startI <= endI){
                otherDataset <- read.csv(formatFileName(idIndex[startI],directory), header=TRUE)
                binded <- rbind(binded,otherDataset)
                startI<-startI+1
        }
        binded
}

formatFileName <- function(fileName, directory){

        if(nchar(fileName) == 1){
                path <- paste(getwd(),"/",directory,"/","00",fileName,".csv",sep="")
        }
        else if(nchar(fileName) == 2) {
                path <- paste(getwd(),"/",directory,"/","0",fileName,".csv",sep="")  
        }else if(nchar(fileName) == 3){
                path <- paste(getwd(),"/",directory,"/",fileName,".csv",sep="")
        }
}