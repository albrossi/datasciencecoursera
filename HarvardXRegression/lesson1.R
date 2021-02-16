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
    
    
    
    