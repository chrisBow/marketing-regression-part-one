library(readr)
library(dplyr)
library(ggplot2)


cost_vs_cpc <- read_csv("cost_vs_cpc.csv")

cost_vs_cpc$Date <- as.Date(cost_vs_cpc$Date, "%m/%d/%Y")

glimpse(cost_vs_cpc)

cost_vs_cpc %>%
  group_by(Date) %>%
  summarise(cost = sum(Spend),
            Clicks = sum(Clicks)) %>%
  mutate(cpc = cost / Clicks) %>%
  ggplot(aes(cost, cpc)) +
  geom_point() +
  geom_smooth()



cost_vs_cpc %>%
  group_by(Date) %>%
  summarise(cost = sum(Spend),
            Clicks = sum(Clicks)) %>%
  mutate(cpc = cost / Clicks) %>%
  ggplot(aes(cost, Clicks)) +
  geom_point() +
  geom_smooth()







cost_summary <-
  
  cost_vs_cpc %>%
  
  group_by(Date) %>%
  
  summarise(cost = sum(Spend),
            
            Clicks = sum(Clicks)) %>%
  
  mutate(cpc = cost / Clicks)





lm_cost_cpc <- lm(cpc ~ cost, data = cost_summary)



lm_cost_cpc



summary(lm_cost_cpc)



cost_summary$cost2 <- cost_summary$cost^2



cost_summary$cost3 <- cost_summary$cost^3



quad <- lm(cpc ~ cost + cost2, data = cost_summary)



cube <- lm(cpc ~ cost + cost2 + cost3, data = cost_summary)



summary(quad)



plot(lm_quad)



library(broom)



aug_mod <- augment(quad)



ggplot(aug_mod, aes(cost, cpc)) +
  
  geom_point()+
  
  geom_line(aes(y = .fitted)) +
  
  theme_minimal()



summary(cube)



aug_mod_3 <- augment(cube)



ggplot(aug_mod_3, aes(cost, cpc)) +
  
  geom_point()+
  
  geom_line(aes(y = .fitted)) +
  
  theme_minimal()



anova(lm_cost_cpc, quad)



quad_test <- lm(cpc ~ cost + (cost^2), data = cost_summary)

summary(quad_test)







# non-linear?



nls_mod <- nls(cpc ~ a * cost / (b + cost), data = cost_summary)



nl_mod <- augment(nls_mod)



ggplot(nl_mod, aes(cost, cpc)) +
  
  geom_point()+
  
  geom_line(aes(y = .fitted)) +
  
  theme_minimal()



anova(lm_cost_cpc, quad)



summary(nls_mod)

summary(quad)

