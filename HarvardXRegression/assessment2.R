library(dslabs)
library(tidyverse)
data("research_funding_rates")
research_funding_rates


    tabela <- research_funding_rates %>% 
      mutate(not_award_men = applications_men - awards_men,
             not_award_women = applications_women - awards_women) %>%
      summarize(awards = sum(awards_men),
                not_award = sum(not_award_men),
                awards_women_total = sum(awards_women),
                not_award_women_total = sum(not_award_women)) %>%
      pivot_longer(c("awards", "not_award"), 
                   names_to = "award", values_to = "men") %>%
      pivot_longer(c("awards_women_total", "not_award_women_total"),
                   values_to = "women") %>%
      select(award, men, women) %>%
      slice(1,4)
      
  
    tabela 
      290/(1345+290)
      177/(177+1011)
      
    tabela %>% select(-award) %>% chisq.test()
      
      
     research_funding_rates %>% 
        mutate(not_award_men = applications_men - awards_men,
               not_award_women = applications_women - awards_women) %>%
        summarize(awards = sum(awards_men),
                  not_award = sum(not_award_men),
                  awards_women_total = sum(awards_women),
                  not_award_women_total = sum(not_award_women)) %>%
        summarize(percent_men = awards/(awards+not_award),
                  percent_women = awards_women_total/(awards_women_total+not_award_women_total))
      
     research_funding_rates %>% 
       mutate(not_award_men = applications_men - awards_men,
              not_award_women = applications_women - awards_women) %>%
       summarize(awards = sum(awards_men),
                 not_award = sum(not_award_men),
                 awards_women_total = sum(awards_women),
                 not_award_women_total = sum(not_award_women)) %>%
       summarize(percent_total = 
                   (awards + awards_women_total)/
                   (awards + not_award +awards_women_total + not_award_women_total))
     
     
     
      
#Question 4
     
     dat <- research_funding_rates %>% 
       mutate(discipline = reorder(discipline, success_rates_total)) %>%
       rename(success_total = success_rates_total,
              success_men = success_rates_men,
              success_women = success_rates_women) %>%
       gather(key, value, -discipline) %>%
       separate(key, c("type", "gender")) %>%
       spread(type, value) %>%
       filter(gender != "total")
     dat
     
     dat %>% ggplot(aes(success, discipline, col = gender, size = applications)) + geom_point()
     
     dat %>% ggplot(aes(discipline, success, size = applications, color = gender)) + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
       geom_point()
     
     dat %>% 
       group_by(discipline) %>%
       summarize(overall = sum(success))  %>%
       arrange(overall)
      