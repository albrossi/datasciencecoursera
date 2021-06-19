library(stargazer)
library(dplyr)
library(ggplot2)
      
      #pull the data
      dataset <- read.csv("C:/RStudio/datasciencecoursera/Udemy-Econometrics/njmin3.csv")
      summary(dataset)
      View(dataset)
      #taking care of missing data
      dataset$fte <- ifelse(is.na(dataset$fte),
                            mean(dataset$fte, na.rm = TRUE),
                            dataset$fte)
      dataset$demp <- ifelse(is.na(dataset$demp),
                             mean(dataset$demp, na.rm = TRUE),
                            dataset$demp)
      summary(dataset)
      
      #Grafico dos dados
      dataset %>%
         group_by(NJ, POST_APRIL92) %>%
         summarise(FTEs = sum(fte, na.rm = T)) %>%
         ggplot() + geom_point(aes(x=POST_APRIL92, y=FTEs, color=NJ))
      
      
      #create the first regression model
      model1 <- lm(fte ~ NJ + POST_APRIL92 + NJ_POST_APRIL92,
                   data = dataset)
      summary(model1)
      
      #create 2nd regression model
      model2 <- lm(fte ~ NJ + POST_APRIL92 + NJ_POST_APRIL92 +
                     bk + kfc + wendys,
                   data = dataset)
      summary(model2)
      
      #Create third regression model
      model3 <- lm(fte ~ NJ + POST_APRIL92 + NJ_POST_APRIL92 +
                     bk + kfc + wendys +
                     co_owned + centralj + southj,
                   data = dataset)
      summary(model3)
      
      stargazer(model1, model2, model3,
                type = "text",
                title = "Impact of minimum wage on employment",
                no.space = TRUE,
                keep.stat = "n",
                digits = 2)
      
      
      
      
      
      
      
      
      
