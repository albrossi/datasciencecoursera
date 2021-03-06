library(broom)
library(tidyverse)


    # generate the Monte Carlo simulation
    N <- 25
    g <- 1000000
    sim_data <- tibble(group = rep(1:g, each = N), x = rnorm(N * g), y = rnorm(N * g))
    
    # calculate correlation between X,Y for each group
    res <- sim_data %>% 
      group_by(group) %>% 
      summarize(r = cor(x, y)) %>% 
      arrange(desc(r))
    res
    
    # plot points from the group with maximum correlation
    sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
      ggplot(aes(x, y)) +
      geom_point() + 
      geom_smooth(method = "lm")
    
    # histogram of correlation in Monte Carlo simulations
    res %>% ggplot(aes(x=r)) + geom_histogram(binwidth = 0.1, color = "black")
    
    # linear regression on group with maximum correlation
    sim_data %>% 
      filter(group == res$group[which.max(res$r)]) %>%
      do(tidy(lm(y ~ x, data = .)))
    
    
    
    
    set.seed(1985)
    x <- rnorm(100,100,1)
    y <- rnorm(100,84,1)
    
    qplot(x, y)
    cor(x,y)
    rank(x)
    
    qplot(rank(x), rank(y))
    cor(x, y, method = "spearman")
    
    
    
# cause and effect reversal using son heights to predict father heights
    library(HistData)
    data("GaltonFamilies")
    GaltonFamilies %>%
      filter(childNum == 1 & gender == "male") %>%
      select(father, childHeight) %>%
      rename(son = childHeight) %>% 
      do(tidy(lm(father ~ son, data = .)))
    
    
# UC-Berkeley admission data
    library(dslabs)
    data(admissions)
    admissions
    
    as_tibble(admissions)
# percent men and women accepted
    admissions %>% group_by(gender) %>% 
      summarize(percentage = 
                  round(sum(admitted*applicants)/sum(applicants),1))
    
# test whether gender and admission are independent
    admissions %>% group_by(gender) %>% 
      summarize(total_admitted = round(sum(admitted / 100 * applicants)), 
                not_admitted = sum(applicants) - sum(total_admitted)) %>%
      select(-gender) %>% 
      do(tidy(chisq.test(.)))
    
# percent admissions by major
    admissions %>% select(major, gender, admitted) %>%
      spread(gender, admitted) %>%
      mutate(women_minus_men = women - men)
    
# plot total percent admitted to major versus percent women applicants
    admissions %>% 
      group_by(major) %>% 
      summarize(major_selectivity = sum(admitted * applicants) / sum(applicants),
                percent_women_applicants = sum(applicants * (gender=="women")) /
                  sum(applicants) * 100) %>%
      ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
      geom_text()
    
# plot number of applicants admitted and not
    admissions %>%
      mutate(yes = round(admitted/100*applicants), no = applicants - yes) %>%
      select(-applicants, -admitted) %>%
      gather(admission, number_of_students, -c("major", "gender")) %>%
      ggplot(aes(gender, number_of_students, fill = admission)) +
      geom_bar(stat = "identity", position = "stack") +
      facet_wrap(. ~ major)
    
    admissions %>% 
      mutate(percent_admitted = admitted * applicants/sum(applicants)) %>%
      ggplot(aes(gender, y = percent_admitted, fill = major)) +
      geom_bar(stat = "identity", position = "stack")
    
# condition on major and then look at differences
    admissions %>% ggplot(aes(major, admitted, col = gender, size = applicants)) + geom_point()
    
# average difference by major
    admissions %>%  group_by(gender) %>% summarize(average = mean(admitted))
    