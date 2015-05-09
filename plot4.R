Plot4 <- function() {
  
  Proj1_Data_Munge <- function () {
    #quick script to document and perform data munging for Coursera Exploratory Data Analysis class
    #function takes a dataframe, converts the Date column to the R Date class, then subsets using dplyr
    #function returns filtered dataframe specific to conditions on course project 1 specifications
    
    #dependancies, wrap side effect code in try/except later
    library(dplyr)
    
    #download, unzip and read txt file as df, wrap side effect prone code in try/except later
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", "Electric_Power_Consumption.csv", method = "curl")
    unzip("Electric_Power_Consumption.csv")
    df <- read.table("household_power_consumption.txt", header=T, sep=c(";"), na.strings=c("?"))
    
    #should fix this repetitive code later if script will be expanded on
    df$Date <- strptime(df$Date, format = "%d/%m/%Y")
    df$Date <- as.Date(df$Date)
    return_dataframe <- filter(df.date, Date == "2007-02-01" | Date == "2007-02-02")
    return_dataframe
  }
  
  generatePlot4 <- function (dataframe) {
    #function takes filtered dataset and generates plot 4 for coursera course Exploratory Data Analysis, Project 1
    par(mfrow = c(2, 2))
    
    #1st plot packing top left to top right, I wanted to just call plot2.R but labels are different
    with(dataframe, plot(Global_active_power ~ as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"), ylab = "Global Active Power", type = "l",  xlab=""))
    
    #2nd plot packing top right
    with(dataframe, {
      plot(Voltage ~ as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"), xlab="datetime", ylab = "Voltage", type = "l")
    })
    
    #3rd plot packing bottom left
    generatePlot3(dataframe)
    
    #4th plot packing bottom right
    with(dataframe, {
      plot(Global_reactive_power ~ as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"), xlab="datetime", type = "l")
    })
  }
  
  #generate plot
  png("plot4.png", width = 480, height = 480)
  df <- Proj1_Data_Munge()
  generatePlot4(df)
  
  #again
  dev.off()
  generatePlot4(df)
}
