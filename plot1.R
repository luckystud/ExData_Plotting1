# Create a data frame from the "household_power_consumption.txt file
# from Feb 1, 2007 to Feb 2, 2007.
get_df <- function(startdate) {
    con <- file("household_power_consumption.txt", open = "r")
    # First line is header
    oneLine <- readLines(con, n = 1, warn = FALSE)
    tmpHeaderVec <- (strsplit(oneLine, ";"))[[1]]
    headerVec <- c("Timestamp", tmpHeaderVec[3:9])
    
    # Set days to collect data
    day1 <- as.Date(startdate, "%m/%d/%Y")
    day2 <- day1 + 1
    
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

# Plot a histogram that matches the example from the assignements web page.
plot1 <- function() {
    data_df <- get_df("02/01/2007")
    png("plot1.png", width = 480, height = 480, units = "px")
    hist(data_df$Global_active_power, 
         col = "Red", 
         xlab = "Global Active Power (kilowatts)", 
         main = "Global Active Power")
    dev.off()
}