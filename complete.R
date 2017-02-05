complete <- function(directory, id = 1:332) {
        # Rewritten 2016 July based on work submitted late 2015

        # Save original working directory and revert at end of function.

        default_Dir <- getwd()

        # Set working directory to argument 'directory'

        setwd(directory)

        # Reformat argument 'id' to file naming standard

        file_List <- paste(formatC(id, width = 3, flag = "0"), ".csv", sep = "")

        # rbind character vector 'list_Files' with lapply and read.csv function

        pollutant_Df <- do.call("rbind",
                                lapply(file_List, read.csv, header = TRUE))

        # Split list variable 'id_List'

        id_List <- split(pollutant_Df, pollutant_Df$ID)

        # Create logical vector of complete cases equal TRUE

        id_Logical <- lapply(id_List, complete.cases)

        # Sum logical vector for number of complete observations

        id_Nobs <- lapply(id_Logical, sum)

        # Create data frame from list's IDs and a vector of observations

        nobs_Df <- data.frame(names(unlist(id_Nobs)), as.vector(unlist(id_Nobs)))

        # Rename dataframe with specified column names

        names(nobs_Df) <- c("id", "nobs")

        # Override data frame order from default ascending to argument 'id'
        # order. Required for case where 'id' = 30:25

        nobs_Df2 <- nobs_Df[match(id, nobs_Df$id), ]

        # Have data frame printed with ascending number row names, required for
        # case where 'id' = 30:25

        rownames(nobs_Df2) <- NULL

        # Return nobs_Df2

        return(nobs_Df2)

        # Return working directory to default

        setwd(default_Dir)

}
