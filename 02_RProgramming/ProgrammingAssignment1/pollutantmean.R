pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        pollutantSum = 0
        pollutantN = 0
        for(i in id){
                fileName = paste(directory, paste(sprintf("%03d",i),"csv", sep="."), sep="/")
                data <- read.csv(file=fileName)[pollutant]
                pollutantSum = pollutantSum + sum(data, na.rm=TRUE)
                pollutantN = pollutantN + sum(!is.na(data))
        }
        pollutantSum/pollutantN
}

