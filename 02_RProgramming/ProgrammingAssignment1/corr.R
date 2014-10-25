corr <- function(directory, threshold = 0) {
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        
        ## 'threshold' is a numeric vector of length 1 indicating the
        ## number of completely observed observations (on all
        ## variables) required to compute the correlation between
        ## nitrate and sulfate; the default is 0
        
        ## Return a numeric vector of correlations
        
        cdata <-  complete(directory)
        ids <- cdata["id"][cdata["nobs"] > threshold]
        
        cr = numeric(0)
        for(i in ids){
                fileName <- paste(directory, paste(sprintf("%03d",i),"csv", sep="."), sep="/")
                data <- read.csv(file=fileName)
                cr <-append(cr, cor(data["sulfate"], data["nitrate"], use="pairwise.complete.obs" ))
        }
        cr
}