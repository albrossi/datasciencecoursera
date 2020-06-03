  
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
  
  
  # Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
  # Using the base plotting system, make a plot showing the total PM2.5 emission 
  # from all sources for each of the years 1999, 2002, 2005, and 2008.

  totalyears <-  unique(NEI$year)
  totalemissions <- tapply(NEI$Emissions, NEI$year, sum)
  
  plot(totalyears, totalemissions, type="l", 
       xlab = "Year", ylab = "Total Emissions",
       main = "Total Emissons per Year")  
  
  
  dev.copy(png,"plot1.png", width=480, height=480)
  dev.off()
  
  
  