library(dplyr)
library(ggplot2)
library(purrr)
library(broom)
library(reshape2)
library(tidyr)

votes <- votes %>% 
  # create quintile variables by race
  mutate(white_alone_q    = ntile(white_alone, 5),
         white_q          = ntile(white, 5),
         black_q          = ntile(black, 5),
         asian_q          = ntile(asian, 5),
         hisp_latin_q     = ntile(hisp_latin, 5)) %>% 
  # create quintile variables by education
  mutate(edu_batch_q      = ntile(edu_batchelors, 5)) %>% 
  # create quintile variables by Housing_units_multistruct
  mutate(urban_q          = ntile(housing_units_multistruct, 5)) %>% 
  # create quintile variables by Income
  mutate(income_q         = ntile(income, 5))

# race_q <- votes %>%
#   select(Clinton, Trump, 
#          white_alone_q, white_q, 
#          black_q, asian_q, 
#          hisp_latin_q, 
#          pref_cand_T) %>%
#   group_by(black_q, white_alone_q) %>%
#   summarize(trump_pref_cand = mean(Trump > Clinton)) %>%
#   ungroup()
# 
# ggplot(race_q, aes(white_alone_q, black_q)) +
# geom_tile(aes(fill = trump_pref_cand)) +
# scale_fill_gradient(low = "blue", high = "red")


####################
# By race analysis #
####################

by_race <- votes %>% 
  group_by(pref_cand_T) %>% 
  summarize(
    white_alone = mean(white_alone),
    white       = mean(white),
    black       = mean(black),
    asian       = mean(asian),
    hisp_latin  = mean(hisp_latin),
    foreign     = mean(foreign)) %>% 
  # reshape to ggplot
  melt(id.vars = 'pref_cand_T')

ggplot(by_race) + 
  geom_bar(aes(x = variable, 
               y = value, 
               fill = pref_cand_T),
           stat = 'identity', 
           position = 'dodge'
  ) + 
  scale_fill_manual(values = alpha(c("blue", "red"))) + 
  labs(
    title = "Proportion of races, hispanic origin or foreign population in counties among candidates",
    subtitle = "(Simple mean of proportion of counties without considering county population)",
    x = "Race",
    y = "Proportion in counties",
    fill = "Preferred Candidate") +
  theme(legend.position = "bottom")

ggplot(by_race, aes(x = pref_cand_T, y = value)) + 
geom_col(aes(fill = pref_cand_T)) + 
facet_wrap(~variable
             # , scales = "free_y"
           ) + 
scale_fill_manual(values = alpha(c("blue", "red"))) 

#######################
# Black race analysis #
#######################

votes %>% 
  group_by(pref_cand_T, black_q) %>% 
  summarize(mean_black = mean(black)) %>% 
  ggplot() + 
  geom_bar(aes(x = black_q, y = mean_black, fill = pref_cand_T),
           stat = 'identity', 
           position = 'dodge'
  ) + 
  scale_fill_manual(values = alpha(c("blue", "red"))) +
  labs(
    title = "Proportion of black race in counties, among counties ordered by Black race population",
    subtitle = "(Simple mean of proportion of counties without considering county population)",
    x = "Black Race Quintile",
    y = "Mean of Percentage of black people") 

#######################
# White race analysis #
#######################

votes %>% 
  group_by(pref_cand_T, white_alone_q) %>% 
  summarize(mean_white_alone = mean(white_alone)) %>% 
  ggplot() + 
  geom_bar(aes(x = white_alone_q, y = mean_white_alone, fill = pref_cand_T),
           stat = 'identity', 
           position = 'dodge'
  ) + 
  scale_fill_manual(values = alpha(c("blue", "red"))) +
  labs(
    title = "Proportion of white race in counties, among counties ordered by white race population",
    subtitle = "(Simple mean of proportion of counties without considering county population)",
    x = "White Race Quintile",
    y = "Mean of Percentage of white people") 

######################################
# Thinking about some correlation... 
######################################

votes %>% 
  nest(-urban_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand_T ~ black, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "black") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(urban_q)

votes %>% 
  nest(-income_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand_T ~ black, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "black") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(income_q)

votes %>%
  nest(-edu_batch_q) %>%
  mutate(models = map(data, ~ glm(pref_cand_T ~ black, .,
                                  family = "binomial"))) %>%
  unnest(map(models, tidy)) %>%
  filter(term == "black") %>%
  filter(p.adjust(p.value) < .05) %>%
  arrange(edu_batch_q)

votes %>%  
  nest(-income_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand_T ~ white_alone, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "white_alone") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(income_q)