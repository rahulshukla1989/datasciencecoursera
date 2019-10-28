## This is a markdown file
require(readxl)
rm(list = ls())
setwd("E:/coursera-practice/R-practice")



pollmean<- function(directory,pollutant, id= 1:332)
{
    means <- c()
    
    for(monitor in id){
        path <- paste(getwd(), "/", directory, "/", sprintf("%03d", monitor), ".csv", sep = "")
        monitor_data <- read.csv(path)
        interested_data <- monitor_data[pollutant]
        means <- c(means, interested_data[!is.na(interested_data)])
    }
    
    mean(means)
}
pollmean("specdata", "nitrate")

complete<- function(directory,id= 1:332)
{

        nobs = numeric()
    
    for(i in id){
        path <- paste(getwd(), "/", directory, "/", sprintf("%03d", i), ".csv", sep = "")
        monitor_data <- read.csv(path)
        nobs=c(nobs,sum(complete.cases(monitor_data)))
    }
    
    return(data.frame(id,nobs))
    
  
}
RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])
corr<- function(directory,threshold = 0)
{

    df = complete(directory)
    ids = df[df["nobs"] > threshold, ]$id
    corrr = numeric()
    
    for(i in ids){
        path <- paste(getwd(), "/", directory, "/", sprintf("%03d", i), ".csv", sep = "")
        monitor_data <- read.csv(path)
        dff=monitor_data[complete.cases(monitor_data), ]
        corrr = c(corrr, cor(dff$sulfate, dff$nitrate))
    }
    
    return(corrr)
    
  
}

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))