# Create a data frame from the "household_power_consumption.txt file
# from Feb 1, 2007 to Feb 2, 2007.
get_df <- function() {
    con <- file("household_power_consumption.txt", open = "r")
    # First line is header
    oneLine <- readLines(con, n = 1, warn = FALSE)
    tmpHeaderVec <- (strsplit(oneLine, ";"))[[1]]
    headerVec <- c("Timestamp", tmpHeaderVec[3:9])
    
    # Set days to collect data
    day1 <- as.Date("02/01/2007", "%m/%d/%Y")
    day2 <- as.Date("02/02/2007", "%m/%d/%Y")
    
    # Initialize vectors
    timeVec <- c()
    gapVec <- c()
    grpVec <- c()
    voltVec <- c()
    giVec <- c()
    sm1Vec <- c()
    sm2Vec <- c()
    sm3Vec <- c()
    ind <- 1;
    
    # Collect only data needed for plotting
    while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
        if (ind %% 100000 == 0) {
            print(paste("Handled ", ind, " lines"))
        }
        ind <- ind + 1;
        dataVec <- (strsplit(oneLine, ";"))[[1]]
        dataDate <- as.Date(dataVec[1], "%d/%m/%Y")
        if (dataDate == day1 | dataDate == day2) {
            timestampStr <- paste(dataVec[1], dataVec[2], sep = " ")
            timestamp <- strptime(timestampStr, "%d/%m/%Y %H:%M:%S")
            if (length(timeVec) == 0) {
                timeVec = as.POSIXlt(timestamp)
            } else {
                timeVec = c(timeVec, timestamp)
            }
            if (dataVec[3] == "?") {
                gapVec <- c(gapVec, NA)
            } else {
                gapVec <- c(gapVec, as.numeric(dataVec[3]))
            }
            if (dataVec[4] == "?") {
                grpVec <- c(grpVec, NA)
            } else {
                grpVec <- c(grpVec, as.numeric(dataVec[4]))
            }
            if (dataVec[5] == "?") {
                voltVec <- c(voltVec, NA)
            } else {
                voltVec <- c(voltVec, as.numeric(dataVec[5]))
            }
            if (dataVec[6] == "?") {
                giVec <- c(giVec, NA)
            } else {
                giVec <- c(giVec, as.numeric(dataVec[6]))
            }
            if (dataVec[7] == "?") {
                sm1Vec <- c(sm1Vec, NA)
            } else {
                sm1Vec <- c(sm1Vec, as.numeric(dataVec[7]))
            }
            if (dataVec[8] == "?") {
                sm2Vec <- c(sm2Vec, NA)
            } else {
                sm2Vec <- c(sm2Vec, as.numeric(dataVec[8]))
            }
            if (dataVec[9] == "?") {
                sm3Vec <- c(sm3Vec, NA)
            } else {
                sm3Vec <- c(sm3Vec, as.numeric(dataVec[9]))
            }
        }
    }
    close(con)
    df <- data.frame(timeVec, gapVec, grpVec, voltVec, giVec, sm1Vec, sm2Vec, sm3Vec)
    colnames(df) <- headerVec
    df
}

plot4 <- function() {
    data_df <- get_df()
    png("plot4.png", width = 480, height = 480, units = "px")
    par(mfrow = c(2, 2), cex = 0.7)
    with(data_df, {
        plot(Timestamp, Global_active_power, type = "l", xlab = NA, ylab = "Global Active Power")
        plot(Timestamp, Voltage, type = "l", xlab = "datetime")
        plot(Timestamp, Sub_metering_1, type = "l", xlab = NA, ylab = "Energy sub metering")
        lines(Timestamp, Sub_metering_2, type = "l", col = "Red")
        lines(Timestamp, Sub_metering_3, type = "l", col = "Blue")
        legend("topright", lty = 1, col = c("Black", "Red", "Blue"), 
               legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
        plot(Timestamp, Global_reactive_power, type = "l", xlab = "datetime")
    })
    dev.off()
}