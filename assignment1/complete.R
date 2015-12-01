complete <- function(directory, id = 1:332) {
    
    # I'm just thinking too imperative and not functional enough :(
    id_v <- c()
    nobs_v <- c()
    
    for (i in id) {
        f <- file.path(directory, sprintf("%03d.csv", i))
        d <- read.csv(f)
        id_v <- c(id_v, i)
        nobs_v <- c(nobs_v, sum(complete.cases(d)))
    }
    
    data.frame(id=id_v, nobs=nobs_v)
}