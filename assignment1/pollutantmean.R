pollutantmean <- function(directory, pollutant, id = 1:332) {
    
    # We are assuming the files are sequentially numbered and not missing any values so
    # that filename[i] == sprintf(file.path(directory), sprintf("%03d, i)))
    
    filenames = list.files(directory, full.names=TRUE)
    
    stationdata <- do.call(rbind, lapply(filenames[id], read.csv))

    # No need, just use the na.rm argument to mean()
    #r <- !is.na(stationdata)
    #print(mean(stationdata[r]))
    
    mean(stationdata[[pollutant]], na.rm=TRUE)
    
}
