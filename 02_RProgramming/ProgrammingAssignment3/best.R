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
        data <-  data[state == data$State, c("Hospital.Name", outcome)]
        data[, outcome] <- as.numeric(data[, outcome])
        minval <- min(data[,outcome], na.rm = TRUE)
        data <- data[data[outcome]== minval & !is.na(data[outcome]),]
        data[order(data$Hospital.Name),"Hospital.Name"][1]
        
}