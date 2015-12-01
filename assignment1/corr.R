corr <- function(directory, threshold = 0) {
    
    corr_data <- vector(mode="numeric")
    
    for (f in list.files(directory)) {
        p <- file.path(directory, f)
        d <- read.csv(p)
        if (sum(complete.cases(d)) > threshold) {
            corr_data <- c(corr_data, cor(d$nitrate, d$sulfate, use="complete.obs"))
        }
    }
    corr_data
    
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
}
