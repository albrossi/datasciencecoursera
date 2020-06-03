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
  
  
  # Across the United States, how have emissions from 
  # coal combustion-related sources changed from 1999–2008?

  SCCcoal <- SCC[grepl("coal", SCC$Short.Name, ignore.case = T),]
  NEIcoal <- NEI[NEI$SCC %in% SCCcoal$SCC,]
  totalCoal <- aggregate(Emissions ~ year + type, 
                         NEIcoal, 
                         sum)
  
  ggplot(totalCoal, aes(year, Emissions, col = type)) +
          geom_line() +
          geom_point() +
          ggtitle(expression("Total US Coal Emissions - Type and Year")) +
          xlab("Year") +
          ylab(expression("US Coal Emissions")) +
          scale_colour_discrete(name = "Type of sources") +
          theme(legend.title = element_text(face = "bold"))
  
  dev.copy(png,"plot4.png", width=480, height=480)
  dev.off()
  
  
