pollutantmean <- function(directory, pollutant, id = 1:332) {

        ## Optimized version of code written late 2015
        
        ## Save original working directory for reversion at end of function
        
        default_Dir <- getwd()
        
        ## Set current working directory to argument 'directory'
        
        setwd(directory)
        
        ## Create list of files for import based on argument 'id'
        
        file_List <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "")
        
        ## rbind an lapply of read.csv on 'file_list'
        
        pollutant_Df <- do.call("rbind", 
                                lapply(file_List, read.csv, header = TRUE))
        
        ## Depending on value of argument 'pollutant', calculate mean
        
        if( tolower(pollutant) == "nitrate" ) {
                mean_nitrate <- mean(pollutant_Df[, 3], na.rm = TRUE)
                print(mean_nitrate)
        } else {
                if( tolower(pollutant) == "sulfate") {
                        mean_sulfate <- mean(pollutant_Df[, 2], na.rm = TRUE)
                        print(mean_sulfate)
                } else {
                        print("Invalid pollutant")
                }
        }
        
        ## Revert working directory back to default
        
        setwd(default_Dir)
}