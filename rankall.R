rankall <- function(outcome, num = "best") {

        # Import data, dropping irrelevant columns

        dat <- read.csv("outcome-of-care-measures.csv", header = TRUE,
                        sep = ",", colClasses = c(rep("character", 2),
                        rep("NULL", 4), "character", rep("NULL", 3),
                        "character", rep("NULL", 5), "character",
                        rep("NULL", 5), "character", rep("NULL", 23)))

        # Define in-scope outcomes, convert "best" to rank 1

        definedoutcomes <- c("heart attack", "heart failure", "pneumonia")
        if (num == "best") {
          num <- 1
          }

        # Define error handling for invalid arguments

        if (!outcome %in% definedoutcomes) {
          stop("invalid outcome")
          }
        if (!(num == "best" | num == "worst" | is.numeric(num))) {
          stop("invalid ranking")
          }

        # Create tidy data

        hearta <- cbind(dat[, 2:4], "heart attack")
        heartf <- cbind(dat[, c(2:3, 5)], "heart failure")
        pneumo <- cbind(dat[, c(2:3, 6)], "pneumonia")

        names(hearta) <- c("hospital", "state", "mortality", "type")
        names(heartf) <- c("hospital", "state", "mortality", "type")
        names(pneumo) <- c("hospital", "state", "mortality", "type")

        dat <- rbind(hearta, heartf, pneumo)

        # Split tidy data into list, drop "Not Available", sort, drop uneeded
        # outcomes, reset rows, then lapply a select of row based on argument
        # 'num' and assign resulting values to 'hospital'.

        dat <- split(dat, dat$state)
        dat <- lapply(dat, FUN = function(x) {
          x[x$mortality != "Not Available", ]
          })
        dat <- lapply(dat, FUN = function(x) {
          x[order(as.numeric(x$mortality), x$hospital), ]
          })
        dat <- lapply(dat, FUN = function(x) {
          x[x$type == outcome, ]
          })
        dat <- lapply(dat, FUN = function(x) {
          rownames(x) <- NULL; x
          })

        hospital <- lapply(dat, FUN = function(x) {
          if (num == "worst") {
            x[dim(x)[1], 1]
            } else {
              x[num, 1]
              }
            })

        state <- names(dat)

        # Create results data frame.

        as.data.frame(cbind(hospital, state))

}
