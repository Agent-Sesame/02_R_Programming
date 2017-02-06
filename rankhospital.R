rankhospital <- function(state, outcome, num = "best") {

        # Import data, dropping irrelevant columns

        dat <- read.csv("outcome-of-care-measures.csv", header = TRUE,
                        sep = ",", colClasses = c(rep("character", 2),
                        rep("NULL", 4), "character", rep("NULL", 3),
                        "character", rep("NULL", 5), "character",
                        rep("NULL", 5), "character", rep("NULL", 23)))

        # Define in-scope outcomes

        definedoutcomes <- c("heart attack", "heart failure", "pneumonia")

        # Define error handling for invalid arguments 'state', 'outcome'

        if (!state %in% unique(dat[, 3])) {
          stop("invalid state")
          }
        if (!outcome %in% definedoutcomes) {
          stop("invalid outcome")
          }
        if (!(num == "best" | num == "worst" | is.numeric(num))) {
          stop("invalid ranking")
          }
        if (!state %in% unique(dat[, 3]) & !outcome %in% definedoutcomes) {
          stop("invalid state and outcome")
          }

        # Subset dat by argument 'state' and simplify column names

        dat <- dat[dat$State == state, ]

        names(dat) <- c("Number", "Hospital", "State", "Heart Attack",
                        "Heart Failure", "Pneumonia")

        # Report ranked hospital according to argument 'outcome' and 'best'

        if (outcome == "heart attack") {

                # Remove 'Not Available', sort, renumber rows, revalue 'num'

                dat <- dat[dat$`Heart Attack` != "Not Available", ]
                dat <- dat[order(as.numeric(dat$`Heart Attack`),
                  dat$Hospital), ]
                rownames(dat) <- NULL
                if (num == "best") {
                  num <- 1
                  }
                if (num == "worst") {
                  num <- dim(dat)[1]
                  }
                print(dat[num, 2])
        }

        if (outcome == "heart failure") {

                dat <- dat[dat$`Heart Failure` != "Not Available", ]
                dat <- dat[order(as.numeric(dat$`Heart Failure`),
                  dat$Hospital), ]
                rownames(dat) <- NULL
                if (num == "best") {
                  num <- 1
                  }
                if (num == "worst") {
                  num <- dim(dat)[1]
                  }
                print(dat[num, 2])
        }

        if (outcome == "pneumonia") {

                dat <- dat[dat$Pneumonia != "Not Available", ]
                dat <- dat[order(as.numeric(dat$Pneumonia), dat$Hospital), ]
                rownames(dat) <- NULL
                if (num == "best") {
                  num <- 1
                  }
                if (num == "worst") {
                  num <- dim(dat)[1]
                  }
                print(dat[num, 2])
        }
}
