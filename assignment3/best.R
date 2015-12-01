#
# Find the best hospital in a given state for a given outcome
#
best <- function(state, outcome) {
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
    
    stateRows <- data$State == state & !is.na(data[,outcome])
    stateData <- data[stateRows, c('Hospital.Name', outcome)]

    x <- order(stateData[,outcome], stateData[,'Hospital.Name'])
    
    ## Return hospital name in that state with lowest 30-day death rate
    head(stateData[x, 'Hospital.Name'], 1)
}
