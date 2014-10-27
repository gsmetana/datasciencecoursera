rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name

        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

        if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
                stop("invalid outcome")
        }
        
        outcome <- switch(outcome,
                          "pneumonia" ="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",
                          "heart failure" ="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", 
                          "heart attack" ="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
        )
        
        state <- unique(data$State)
        state <- state[order(state)]
                
        data <-  data[, c("Hospital.Name", "State", outcome)]
        data[, outcome] <- as.numeric(data[, outcome])   
                
        hospital <- character(0)
        for(i in state){
                tmp <-  data[i == data$State, c("Hospital.Name", outcome)]
                tmp <- tmp[order(tmp[,outcome], tmp$Hospital.Name), ]
                tmp <- tmp[!is.na(tmp[outcome]),]
                
                if(num == "best"){
                        n <- 1
                } else if(num == "worst"){
                        n <- length(tmp$Hospital.Name)
                } else{
                        n = num
                }
                hospital <-append(hospital, tmp[n, "Hospital.Name"] )
        }
        
        result <- data.frame( hospital = hospital, state = state)
        rownames(result) <- state
        result
}