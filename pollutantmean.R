pollutantmean <- function(directory, pollutant, id=1:332){
        alldata<-data.frame(Date=as.Date(character()), sulfate=double(), nitrate=double(),ID=integer())
        for(i in id){
                inputdata <- data.frame(read.csv(file.path(directory,paste(formatC(i,width=3, flag="0"),".csv",sep=""))))
                alldata<-rbind(alldata,inputdata)
               }
        mean(alldata[,pollutant],na.rm=TRUE)
}

complete<-function(directory,id=1:332){
        alldata<-data.frame(Date=as.Date(character()), sulfate=double(), nitrate=double(),ID=integer())
        for(i in id){
                inputdata <- data.frame(read.csv(file.path(directory,paste(formatC(i,width=3, flag="0"),".csv",sep=""))))
                alldata<-rbind(alldata,inputdata)
        }
        nobs<-vector(mode="integer", length=length(id))
        
        for(i in 1:length(id)){
                nobs[i] <- sum(alldata$ID[complete.cases(alldata)]==id[i])
        }
        
        print(data.frame(id,nobs))
}

corr<-function(directory, threshold=0){
        alldata <- data.frame(Date=as.Date(character()), sulfate=double(), nitrate=double(),ID=integer())
        filenames <- list.files(directory)
        cr<-c()
        
        for(i in 1:length(filenames)){
                inputdata <- data.frame(read.csv(file.path(directory,filenames[i])))
                alldata<-rbind(alldata,inputdata)
                alldata <- alldata[complete.cases(alldata),]
                
                if (nrow(alldata) > threshold) {
                        cr <- c(cr, cor(alldata$sulfate, alldata$nitrate) )
                }
        }
}
        
