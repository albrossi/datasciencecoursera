library(ggplot2)
library(RColorBrewer)


  #Reading and confirming data
  filename <- "exdata_data_NEI_data.zip"
  folder <- "exdata_data_NEI_data"
  
  # File already exists?
  if (!file.exists(filename)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    download.file(fileURL, filename)
  }  
  
  # Folder exists?
  if (!file.exists(folder)) { 
    unzip(filename) 
  }
  
  # Data read
  NEI <- readRDS(paste0(folder, "/summarySCC_PM25.rds"))
  SCC <- readRDS(paste0(folder, "/Source_Classification_Code.rds"))
  
  
  # How have emissions from motor vehicle sources changed 
  # from 1999–2008 in Baltimore City?
    
  baltimoreMotor <- subset(NEI, NEI$fips == "24510" & NEI$type == "ON-ROAD")
  baltimoreMotorAGG <- aggregate(Emissions ~ year, baltimoreMotor, sum)
  
  ggplot(baltimoreMotorAGG, aes(year, Emissions)) +
    geom_line(col = "steelblue3") +
    geom_point(col = "steelblue3") +
    ggtitle(expression("Baltimore Motor Vehicle Emissions by Year")) +
    xlab("Year") +
    ylab(expression("Motor Vehicle Emissions"))
  
  
  dev.copy(png,"plot5.png", width=480, height=480)
  dev.off()


