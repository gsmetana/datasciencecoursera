complete <- function(directory, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used
        
        ## Return a data frame of the form:
        ## id nobs
        ## 1  117
        ## 2  1041
        ## ...
        ## where 'id' is the monitor ID number and 'nobs' is the
        ## number of complete 
        
        nobs <- numeric(0)
        for(i in id){
                fileName <- paste(directory, paste(sprintf("%03d",i),"csv", sep="."), sep="/")
                data <- read.csv(file=fileName)
                nobs <-append(nobs, sum(  !is.na(data["sulfate"]) & !is.na(data["nitrate"])  ))
        }
        data.frame( id = id, nobs = nobs)
        
}