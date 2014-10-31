best <- function(state, outcome) {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with lowest 30-day death
        ## rate
                
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
        sdata <-  data[state == data$State, c("Hospital.Name", outcome)]
        
        sdata[order(as.numeric(sdata[, outcome]), sdata[, "Hospital.Name"])[1], "Hospital.Name"]   
}