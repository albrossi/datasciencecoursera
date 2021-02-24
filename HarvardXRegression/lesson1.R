library(Lahman)
library(dplyr)
library(ggplot2)
library(HistData)


    data("GaltonFamilies")
    set.seed(1983)
    
    Teams %>% filter(yearID %in% 1961:2001 ) %>%
          mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
          summarize(r=cor(X2B_per_game,X3B_per_game))
    
    
    
    
    
    galton_heights <- GaltonFamilies %>%
      filter(gender == "male") %>%
      group_by(family) %>%
      sample_n(1) %>%
      ungroup() %>%
      select(father, childHeight) %>%
      rename(son = childHeight)
    
    galton_heights %>% 
      summarize(mean(father), sd(father), mean(son), sd(son))
    
    galton_heights %>% ggplot(aes(father, son)) + 
      geom_point(alpha = 0.5)
    
    
    R <- sample_n(galton_heights, 50, replace = TRUE) %>% 
      summarize(r = cor(father, son)) %>% pull(r)
    
    B <- 1000
    N <- 50
    R <- replicate(B, {
      sample_n(galton_heights, N, replace = TRUE) %>% 
        summarize(r=cor(father, son)) %>% 
        pull(r)
    })
    qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))
    
    
    set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
    data("GaltonFamilies")
    
    female_heights <- GaltonFamilies%>%     
      filter(gender == "female") %>%     
      group_by(family) %>%     
      sample_n(1) %>%     
      ungroup() %>%     
      select(mother, childHeight) %>%     
      rename(daughter = childHeight)  %>% 
      summarize(m_mother=mean(mother),
                sd_mother=sd(mother),
                m_daughter=mean(daughter),
                sd_daughter=sd(daughter),
                r=cor(mother, daughter))
    female_heights
    
    mu_x <- 64.1
    mu_y <- 64.3
    s_x <- 2.29
    s_y <- 2.39
    r   <- 0.325
    r2 <- r^2
    
    slope <- r * s_y/s_x
    intercept <- mu_y - r * s_y/s_x * mu_x
    
    res <-  intercept + 60*slope
    
    
    
    
    
    
    
    
    
    
    
    
    
    