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
  
  
  # Compare emissions from motor vehicle sources in 
  # Baltimore City with emissions from motor vehicle sources 
  # in Los Angeles County, California fips == "06037". 
  # Which city has seen greater changes over time in motor 
  # vehicle emissions?
  
  baltLosAngelesMotors <- subset(NEI, NEI$fips %in% c("24510","06037") & NEI$type == "ON-ROAD")
  baltLosAngelesMotorsAGG <- aggregate(Emissions ~ year + fips, baltLosAngelesMotors, sum)
  
  ggplot(baltLosAngelesMotorsAGG, aes(year, Emissions, col = fips)) +
    geom_line() +
    geom_point() +
    ggtitle(expression("Baltimore and Los Angeles Motor Vehicle Emissions by Year")) +
    labs(x = "Year", y = expression("Motor Vehicle Emissions") ) +
    scale_colour_discrete(name = "City", labels = c("Los Angeles", "Baltimore")) +
    theme(legend.title = element_text(face = "bold"))
  
  
  dev.copy(png,"plot6.png", width=480, height=480)
  dev.off()
  
  
