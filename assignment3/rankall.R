rankall <- function(outcome, num = "best") {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    
    ## Check that outcome is valid
    if (outcome == 'heart attack') {
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'
    } else if (outcome == 'heart failure') {
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'
    } else if (outcome == 'pneumonia') {
        outcome <- 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
    } else {
        stop("invalid outcome")
    }
    
    # Get the NAs so we can exclude them.
    data[,outcome] <- as.numeric(data[,outcome])
    
    data <- data[!is.na(data[,outcome]),]
    
    ## For each state, find the hospital of the given rank
    byState <- split(data, data$State)
    orderedByState <- lapply(byState, function (x) x[order(x[outcome], x$Hospital.Name),] )
    
    wantCols = c('Hospital.Name', 'State')

    getRank <- function (x) {
        if (num == "best") {
            x[1, wantCols]
        } else if (num == "worst") {
            tail(x[, wantCols], 1)
        } else{
            x[num, wantCols]
        }
    }
    
    wantedRanks <- lapply(orderedByState, getRank)
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    outputFrame <- do.call(rbind, wantedRanks)
    
    # Set the column names
    colnames(outputFrame) <- c('hospital', 'state')
    
    # state observation has some <NA> in it, fudge it.
    outputFrame['state'] <- rownames(outputFrame)
    
    outputFrame
    
}