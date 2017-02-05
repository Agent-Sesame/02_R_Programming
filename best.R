best <- function(state, outcome) {

        # Import data, dropping irrelevant columns

        dat <- read.csv("outcome-of-care-measures.csv", header = TRUE,
                        sep = ",", colClasses = c(rep("character", 2),
                                rep("NULL", 4), "character", rep("NULL", 3),
                                "character", rep("NULL", 5), "character",
                                rep("NULL", 5), "character", rep("NULL", 23)))

        # Define in-scope outcomes

        definedoutcomes <- c("heart attack", "heart failure", "pneumonia")

        # Validate function inputted arguments, return error message if req'd

        if (!state %in% unique(dat[, 3])) {
          stop("invalid state")
          }
        if (!outcome %in% definedoutcomes) {
          stop("invalid outcome")
          }
        if (!state %in% unique(dat[, 3]) & !outcome %in% definedoutcomes) {
          stop("invalid state and outcome")
          }

        # Subset data set by argument 'state' and simplify column names

        dat <- dat[dat$State == state, ]
        names(dat) <- c("Number", "Hospital", "State", "Heart Attack",
                        "Heart Failure", "Pneumonia")

        # Report best hospital according to argument 'outcome' with 3 if's.

        if (outcome == "heart attack") {

                ## Remove "Not Available" observations, subset by min number

                dat <- dat[dat$`Heart Attack` != "Not Available", ]
                dat <- dat[which(as.numeric(dat$`Heart Attack`) ==
                                        min(as.numeric(dat$`Heart Attack`))), ]
                print(dat[, 2])
        }

        if (outcome == "heart failure") {
                dat <- dat[dat$`Heart Failure` != "Not Available", ]
                dat <- dat[which(as.numeric(dat$`Heart Failure`) ==
                                        min(as.numeric(dat$`Heart Failure`))), ]
                print(dat[, 2])
        }

        if (outcome == "pneumonia") {
                dat <- dat[dat$Pneumonia != "Not Available", ]
                dat <- dat[which(as.numeric(dat$Pneumonia) ==
                                        min(as.numeric(dat$Pneumonia))), ]
                print(dat[, 2])
        }

}
