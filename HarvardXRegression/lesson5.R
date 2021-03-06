library(HistData)
library(gridExtra)
library(Lahman)
library(broom)
library(tidyverse)



    # stratify by HR
    dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(HR = round(HR/G, 1), 
             BB = BB/G,
             R = R/G) %>%
      select(HR, BB, R) %>%
      filter(HR >= 0.4 & HR<=1.2)
    
    # calculate slope of regression lines to predict runs by BB in different HR strata
    dat %>%  
      group_by(HR) %>%
      summarize(slope = cor(BB,R)*sd(R)/sd(BB))
    
    # use lm to get estimated slopes - lm does not work with grouped tibbles
    dat %>%  
      group_by(HR) %>%
      lm(R ~ BB, data = .) %>%
      .$coef
    
    # inspect a grouped tibble
    dat %>% group_by(HR) %>% head()
    dat %>% group_by(HR) %>% class()


    # inspect data frame and tibble
    Teams
    as_tibble(Teams)
    # Note that the function was formerly called as.tibble()
    
    # subsetting a data frame sometimes generates vectors
    class(Teams[,20])
    
    # subsetting a tibble always generates tibbles
    class(as_tibble(Teams[,20]))
    
    # pulling a vector out of a tibble
    class(as_tibble(Teams)$HR)
    
    # access a non-existing column in a data frame or a tibble
    Teams$hr
    as_tibble(Teams$hr)
    
    as_tibble(Teams)$HR
    
    # create a tibble with complex objects
    teste <- tibble(id = c(1, 2, 3), func = c(mean, median, sd))
    

    # use do to fit a regression line to each HR stratum
    dat %>%  
      group_by(HR) %>%
      do(fit = lm(R ~ BB, data = .))
    
    # using do without a column name gives an error
    dat %>%
      group_by(HR) %>%
      do(lm(R ~ BB, data = .))
    
    # define a function to extract slope from lm
    get_slope <- function(data){
      fit <- lm(R ~ BB, data = data)
      data.frame(slope = fit$coefficients[2], 
                 se = summary(fit)$coefficient[2,2])
    }
    
    # return the desired data frame
    dat %>%  
      group_by(HR) %>%
      do(get_slope(.))
    
    # not the desired output: a column containing data frames
    dat %>%  
      group_by(HR) %>%
      do(slope = get_slope(.))
    
    # data frames with multiple rows will be concatenated appropriately
    get_lse <- function(data){
      fit <- lm(R ~ BB, data = data)
      data.frame(term = names(fit$coefficients),
                 estimate = fit$coefficients, 
                 se = summary(fit)$coefficient[,2])
    }
    
    dat %>%  
      group_by(HR) %>%
      do(get_lse(.))
    
    
    
    # use tidy to return lm estimates and related information as a data frame
    library(broom)
    fit <- lm(R ~ BB, data = dat)
    tidy(fit)
    
    # add confidence intervals with tidy
    tidy(fit, conf.int = TRUE)
    
    # pipeline with lm, do, tidy
    dat %>%  
      group_by(HR) %>%
      do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
      filter(term == "BB") %>%
      select(HR, estimate, conf.low, conf.high)
    
    # make ggplots
    dat %>%  
      group_by(HR) %>%
      do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
      filter(term == "BB") %>%
      select(HR, estimate, conf.low, conf.high) %>%
      ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_errorbar() +
      geom_point()
    
    # inspect with glance
    glance(fit)
    
    
    
    get_slope <- function(data) {
      fit <- lm(R ~ BB, data = data)
      sum.fit <- summary(fit)
      
      data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
                 se = sum.fit$coefficients[2, "Std. Error"],
                 pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
    }
    
    dat %>%  
      group_by(HR) %>%
      do(get_slope(.))

    
    dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
      mutate(HR = HR/G,
             R = R/G) %>%
      select(lgID, HR, BB, R) 
    
    
    dat %>% 
      group_by(lgID) %>% 
      do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
      filter(term == "HR") 
    
    
    library(tidyverse)
    library(HistData)
    data("GaltonFamilies")
    set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
    galton <- GaltonFamilies %>%
      group_by(family, gender) %>%
      sample_n(1) %>%
      ungroup() %>% 
      gather(parent, parentHeight, father:mother) %>%
      mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
      unite(pair, c("parent", "child"))
    
    galton
    
    galton %>% 
      group_by(pair) %>%
      summarize(n())
    
    
    galton %>% 
      group_by(pair) %>%
      summarize(cor = cor(parentHeight, childHeight))
   
    
    galton %>% 
      group_by(pair) %>%
      do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))%>%
      filter(p.value < 0.05)
    
    galton %>% 
      group_by(pair) %>%
      do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))%>%
      filter(term == "parentHeight") %>%
      select(pair, estimate, conf.low, conf.high) %>%
      ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_errorbar() +
      geom_point()
    
    
    galton %>% 
      group_by(pair) %>%
      do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))%>%
      filter(term == "parentHeight") %>%
      select(pair, estimate, conf.low, conf.high) %>%
      mutate(larger = conf.high - conf.low)
    
    