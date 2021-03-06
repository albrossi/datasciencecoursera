# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
library(dplyr)
library(ggplot2)
    data("GaltonFamilies")
    set.seed(1983)
    
    galton_heights <- GaltonFamilies %>%
      filter(gender == "male") %>%
      group_by(family) %>%
      sample_n(1) %>%
      ungroup() %>%
      select(father, childHeight) %>%
      rename(son = childHeight)
    
    rss <- function(beta0, beta1, data){
      resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
      return(sum(resid^2))
    }
    
    # plot RSS as a function of beta1 when beta0=25
    beta1 = seq(0, 1, len=nrow(galton_heights))
    results <- data.frame(beta1 = beta1,
                          rss = sapply(beta1, rss, beta0 = 36))
    results %>% ggplot(aes(beta1, rss)) + geom_line() + 
      geom_line(aes(beta1, rss))
    
    # fit regression line to predict son's height from father's height
    fit <- lm(son ~ father, data = galton_heights)
    fit
    
    # summary statistics
    summary(fit)
    
    # Monte Carlo simulation
    B <- 1000
    N <- 100
    lse <- replicate(B, {
      sample_n(galton_heights, N, replace = TRUE) %>% 
        lm(son ~ father, data = .) %>% 
        .$coef 
    })
    lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
    lse
    # Plot the distribution of beta_0 and beta_1
    library(gridExtra)
    p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
    p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
    grid.arrange(p1, p2, ncol = 2)
    
    
    galton_heights %>% ggplot(aes(father, son)) +
      geom_point() +
      geom_smooth()
    
    galton_heights %>% ggplot(aes(father, son)) +
      geom_point() +
      geom_smooth(method = "lm")
    
    model <- lm(son ~ father, data = galton_heights)
    predictions <- predict(model, interval = c("confidence"), level = 0.95)
    data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)
    
    ggplot(data, aes(x = father, y = fit)) +
      geom_line(color = "blue", size = 1) + 
      geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
      geom_point(data = galton_heights, aes(x = father, y = son))
    
    model <- lm(son ~ father, data = galton_heights)
    predictions <- predict(model)
    data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)
    
    ggplot(data, aes(x = father, y = fit)) +
      geom_line(color = "blue", size = 1) + 
      geom_point(data = galton_heights, aes(x = father, y = son))
    
    # summary statistics
    sample_n(galton_heights, N, replace = TRUE) %>% 
      lm(son ~ father, data = .) %>% 
      summary %>%
      .$coef
    
    lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
    
    lse %>% summarize(cor(beta_0, beta_1))
    
    B <- 1000
    N <- 50
    lse <- replicate(B, {
      sample_n(galton_heights, N, replace = TRUE) %>%
        mutate(father = father - mean(father)) %>%
        lm(son ~ father, data = .) %>% .$coef 
    })
    
    cor(lse[1,], lse[2,]) 
    
    # plot predictions and confidence intervals
    galton_heights %>% ggplot(aes(father, son)) +
      geom_point() +
      geom_smooth(method = "lm")
    
    # predict Y directly
    fit <- galton_heights %>% lm(son ~ father, data = .) 
    Y_hat <- predict(fit, se.fit = TRUE)
    names(Y_hat)
    
    # plot best fit line
    galton_heights %>%
      mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
      ggplot(aes(father, Y_hat))+
      geom_line()

    
library(Lahman)
    
    bb_slope <- Teams %>% 
      filter(yearID %in% 1961:2001 ) %>% 
      mutate(BB_per_game = BB/G, R_per_game = R/G, HR_per_game = HR/G) %>% 
      lm(R_per_game ~ BB_per_game+HR_per_game, data = .) %>% 
      .$coef %>%
      .[2]
    bb_slope    

    
    
    set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
    library(HistData)
    data("GaltonFamilies")
    options(digits = 3)    # report 3 significant digits
    
    female_heights <- GaltonFamilies %>%     
      filter(gender == "female") %>%     
      group_by(family) %>%     
      sample_n(1) %>%     
      ungroup() %>%     
      select(mother, childHeight) %>%     
      rename(daughter = childHeight)
    
    reg <- lm(mother ~ daughter, data = female_heights) 
    summary(reg)
    fit <- lm(mother ~ daughter, data = female_heights) 
    Y_hat <- predict(fit, se.fit = TRUE)
    
    female_heights %>%
      mutate(Y_hat = predict(lm(mother ~ daughter, data=.))) %>%
      ggplot(aes(daughter, Y_hat))+
      geom_point()+
      geom_smooth(method = "lm")
    
    
    library(Lahman)
    bat_01 <- Batting %>% filter(yearID == 2002) %>%
      mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
      filter(pa >= 100) %>%
      select(playerID, singles, bb)
    
    
    
    bat_02 <- Batting %>% filter(yearID %in% 1999:2001) %>%
      mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
      filter(pa >= 100) %>%
      select(playerID, singles, bb) %>%
      group_by(playerID)  %>%
      summarize(mean_singles = mean(singles),
                mean_bb = mean(bb)) 
    
    dat <- inner_join(bat_01, bat_02, by = "playerID")
    
    cor(dat$singles, dat$mean_singles)
    cor(dat$bb, dat$mean_bb)
    
    count(bat_02)
    
    dat %>% ggplot(aes(mean_singles, singles)) +
      geom_point()
    hist(dat$singles)
    hist(dat$mean_singles)
    
    dat %>% ggplot(aes(mean_bb, bb)) +
      geom_point()
    hist(dat$mean_bb)
    hist(dat$bb)
    
    
    fit <- lm(singles ~ mean_singles, data = dat) 
    summary(fit)
    
    fit2 <- lm(bb ~ mean_bb, data = dat) 
    summary(fit2)
    
    Y_hat <- predict(fit2, se.fit = TRUE)
    
    
    
    
    
    
    