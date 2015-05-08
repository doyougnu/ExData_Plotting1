Plot2 <- function() {
  
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
  
  generatePlot2 <- function (dataframe) {
    #function takes filtered dataset and generates plot 2 for coursera course Exploratory Data Analysis, Project 1
    with(dataframe, plot(Global_active_power ~ as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"), ylab = "Global Active Power (kilowatts)", type = "n", xlab=""))
    with(dataframe, lines(Global_active_power ~ as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S")))
  }
  
  #save plot
  png("plot2.png")
  df <- Proj1_Data_Munge()
  generatePlot2(df)
  
  #generate plot again for convience
  dev.off()
  generatePlot2(df)
}
