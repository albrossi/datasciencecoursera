library(HistData)
library(gridExtra)
library(Lahman)
library(broom)
library(tidyverse)
    

#Question 1
    Teams_small <- Teams %>% 
        filter(yearID %in% 1961:2001) %>% 
        mutate(avg_attendance = attendance/G)
    View(Teams_small)
    
    Teams_small %>% 
        mutate(R = R/G) %>%
        do(tidy(lm(avg_attendance ~ R, data = .), conf.int = TRUE))
    
    Teams_small %>% 
      mutate(HR = HR/G) %>%
      do(tidy(lm(avg_attendance ~ HR, data = .), conf.int = TRUE))
    
    Teams_small %>% 
      do(tidy(lm(avg_attendance ~ W, data = .), conf.int = TRUE))
    
    reg <- lm(avg_attendance ~ W, data = Teams_small)
    summary(reg)
    
    imagine <- data.frame(W = c(0))
    prd <- predict(reg, imagine)
    summary(prd)
    
    Teams_small %>% 
      do(tidy(lm(avg_attendance ~ yearID, data = .), conf.int = TRUE))
    
    
    
#Question 2
    
    
    Teams_small %>%
        mutate(R_per_game = R/G, 
               HR_per_game = HR/G) %>%
        summarize(C_W_RG = cor(W, R_per_game),
                  C_W_HRG = cor(W, HR_per_game))
      
    
    
    
#Question 3
    
    dat <- Teams_small %>%
      mutate(W = round(W/10)) %>%
      filter(W %in% 5:10) %>%
      filter(W == 8)
    
    View(dat)
    
    Teams_small %>%
      mutate(W = round(W/10)) %>%
      filter(W %in% 5:10) %>%
      mutate(R_per_game = R/G, 
             HR_per_game = HR/G) %>%
      group_by(W) %>%
      do(tidy(lm(avg_attendance ~ HR_per_game, data = .), conf.int = TRUE)) %>%
      filter(term != "(Intercept)")
    
    
    
    Teams_small %>%
      mutate(W = round(W/10)) %>%
      filter(W %in% 5:10) %>%
      mutate(R_per_game = R/G, 
             HR_per_game = HR/G) %>%
      group_by(W) %>%
      summarise(C_AA_RG = cor(avg_attendance, R_per_game),
                C_AA_HRG = cor(avg_attendance, HR_per_game))
    
    
    
    Teams_small %>%
      mutate(W = round(W/10)) %>%
      filter(W %in% 5:10) %>%
      mutate(R_per_game = R/G, 
             HR_per_game = HR/G) %>%
      group_by(W) %>%
      summarize(HRG = mean(HR_per_game),
                AVG = mean(avg_attendance))
    
    
    
#Question 4
    
    dat <- Teams_small %>%
        mutate(R_per_game = R/G, 
               HR_per_game = HR/G) %>%
        do(tidy(lm(avg_attendance ~ R_per_game+HR_per_game+W+yearID, data = .), conf.int = TRUE)) 
        
    
#Question 5
    
    fit <- Teams_small %>%
      mutate(R_per_game = R/G, 
             HR_per_game = HR/G) %>%
      lm(avg_attendance ~ R_per_game+HR_per_game+W+yearID, data = .)
    summary(fit)   
 
    imagine <- data.frame(R_per_game = c(5),
                          HR_per_game = c(1.2),
                          W = c(80),
                          yearID = c(1960))
    
    prd <- predict(fit, imagine)
    summary(prd)
    

#Question 6
    
    fit <- Teams_small %>%
      mutate(R_per_game = R/G, 
             HR_per_game = HR/G) %>%
      lm(avg_attendance ~ R_per_game+HR_per_game+W+yearID, data = .)
    
    model_test <- Teams %>%
      filter(yearID == 2002)  %>%
      mutate(R_per_game = R/G, 
             HR_per_game = HR/G)  %>%
      mutate(avg_attendance = attendance/G,
             avg_predicted = predict(fit, .)) %>%
      summarise(corr = cor(avg_attendance,avg_predicted))
    
    View(model_test)
    
    