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
  
  
  # Of the four types of sources indicated by the type (point, nonpoint, 
  # onroad, nonroad) variable, which of these four sources have seen
  # decreases in emissions from 1999–2008 for Baltimore City? 
  # Which have seen increases in emissions from 1999–2008? 
  # Use the ggplot2 plotting system to make a plot answer this question.
  
  bct <- subset(NEI, fips == "24510")
  
  totalemissionsBCt <-  aggregate(Emissions ~ year + type, bct, sum)

  ggplot(totalemissionsBCt, aes(year, Emissions, col = type)) + 
          geom_line() +
          geom_point() +
          ggtitle(expression("Total Emissons for Baltimore - Type and Year")) +
          ylab(expression("Total Baltimore Emissions")) +
          xlab("Year") +
          scale_colour_discrete(name = "Type of sources") +
          theme(legend.title = element_text(face = "bold")) 
  
  
  dev.copy(png,"plot3.png", width=480, height=480)
  dev.off()
  
  
