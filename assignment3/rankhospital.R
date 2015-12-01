rankhospital <- function(state, outcome, num = "best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that state and outcome are valid
    if (!any(data$State == state)) {
        stop("invalid state")
    }
    
    outcome <- if (outcome == 'heart attack') {
        'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    } else if (outcome == 'heart failure') {
        'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    } else if (outcome == 'pneumonia') {
        'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    } else {
        stop("invalid outcome")
    }
    
    data[,outcome] <- suppressWarnings(as.numeric(data[,outcome]))
    
    wantRows <- data$State == state & !is.na(data[,outcome])
    
    stateData <- data[wantRows, c('Hospital.Name', outcome)]
    
    x <- order(stateData[,outcome], stateData[,'Hospital.Name'])
    
    orderedNames <- stateData[x, 'Hospital.Name']
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    if (num == "best") {
        return(orderedNames[1])
    } else if (num == "worst") {
        n = length(orderedNames)
        return(orderedNames[n])
    } else {
        return(orderedNames[num])
    }
}
