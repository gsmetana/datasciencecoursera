rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        library(plyr)
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        outcome <- switch(outcome,
                          "pneumonia" ="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                          "heart failure" ="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                          "heart attack" ="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        )
        if(num == "best"){
                num <- 1
        }
        
        # remove tricky NAs
        data[,outcome] <- as.numeric(data[, outcome])
        data <-  data[!is.na(data[,outcome]), c("Hospital.Name","State",outcome)]
        
        # return the proper hospital for each state
        orderHospitals <- function(df){ 
                if(num == "best"){
                        n <- 1
                } else if(num == "worst"){
                        n <- length(df$Hospital.Name)
                } else{
                        n = num
                }
                df[order(df[,outcome], df[,"Hospital.Name"])[n], "Hospital.Name"]
        }
        
        result <- ddply(data, c("State"), orderHospitals )
        
        # format result
        result <- result[, c(2,1) ]
        names(result) <- c("hospital", "state")
        rownames(result) <- result$state
        result
}