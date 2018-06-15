corr <- function(directory, threshold = 0) {

        # Set working dirctory to 'argument' directory

        setwd(directory)

        # Read all files in working directory, appending each to 'data_Df'

        data_Df <- do.call("rbind",
                                lapply(list.files(), read.csv, header = TRUE))

        # Calculation number of complete cases in data_Df

        id_Nobs <- lapply(lapply(split(data_Df, data_Df$ID),
                                complete.cases), sum)

        # Unlist id_Nobs to perform data frame operations on id_Nobs output

        nobs_Df <- data.frame(names(unlist(id_Nobs)),
                                as.vector(unlist(id_Nobs)))

        # Tests if argument 'threshold' greater than maximum id observations

        if (threshold >  max(nobs_Df$as.vector.unlist.id_Nobs..)) {

                # When 'threshold' greater, print 0 length numeric vector

                print(numeric(length = 0))

        } else {

                # When 'threshold' less, subset 'data_Df' by nobs_Df ids

                th_Df <- data_Df[data_Df$ID %in%
                                as.integer(as.vector(unlist(nobs_Df[
                                nobs_Df$as.vector.unlist.id_Nobs.. >
                                threshold, ][1]))), ]

                # Calculate sulfate nitrate cor by id for ids in th_Df

                id_Cor <- lapply(split(th_Df, th_Df$ID),
                                function(x) cor(x$sulfate, x$nitrate,
                                use = "complete.obs"))

                # Print results

                print(round(as.vector(unlist(id_Cor)), 5))

        }

}
