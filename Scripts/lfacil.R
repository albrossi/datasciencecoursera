#Reading and confirming data
filename <- "C:/RStudio/datasciencecoursera/Data/lfacil.csv"


# Data read
dados <- read.csv(filename,
                  header = TRUE, 
                  sep = ";",
                  blank.lines.skip = TRUE)

dados <- na.omit(dados)
listadados <- c(dados$Bola1,
                dados$Bola2,
                dados$Bola3,
                dados$Bola4,
                dados$Bola5,
                dados$Bola6,
                dados$Bola7,
                dados$Bola8,
                dados$Bola9,
                dados$Bola10,
                dados$Bola11,
                dados$Bola12,
                dados$Bola13,
                dados$Bola14,
                dados$Bola15)

z<-hist(listadados, breaks=50)


