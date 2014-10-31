rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        if(!state %in% data$State){
                stop("invalid state")
        }
        outcome <- switch(outcome,
                          "pneumonia" ="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                          "heart failure" ="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                          "heart attack" ="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        )
        
        # focus on data for given state
        data[,outcome] <- as.numeric(data[, outcome])
        sdata <-  data[!is.na(data[,outcome]) & state == data$State, c("Hospital.Name", outcome)]
        
        if(num == "best"){
                num <- 1
        } else if(num == "worst"){
                num <- length(sdata$Hospital.Name)
        }               
        sdata[order(sdata[, outcome], sdata[, "Hospital.Name"])[num], "Hospital.Name"]   
        
}