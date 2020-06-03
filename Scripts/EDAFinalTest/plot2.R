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
  
  
  # Have total emissions from PM2.5 decreased in the Baltimore City, 
  # Maryland (fips == "24510") 
  # from 1999 to 2008? Use the base plotting system to make a plot 
  # answering this question.
  
  bc <- subset(NEI, fips == "24510")
  
  totalyearsBC <-  unique(bc$year)
  totalemissionsBC <- tapply(bc$Emissions, bc$year, sum)
  
  plot(totalyearsBC, totalemissionsBC, type="l", 
       xlab = "Year", ylab = "Total Emissions",
       main = "Total Emissons per Year for Baltimore City")  
  
  
  dev.copy(png,"plot2.png", width=480, height=480)
  dev.off()
  
  
