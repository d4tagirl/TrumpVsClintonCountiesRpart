rm(list = ls()) 
load(("to_publish.Rdata"))

library(dplyr)
library(ggplot2)
library(purrr)
library(broom)
library(reshape2)
library(tidyr)

# check
votes %>%
  summarize(Clinton_counties = mean(Clinton >= 0.5, na.rm = TRUE),
            Trump_counties = mean(Trump >= 0.5, na.rm = TRUE),
            trump_pref_cand = mean(Trump > Clinton)) 

# race_q <- votes %>% 
#   select(Clinton, Trump, white_q, black_q, asian_q, hispanic_q, pref_cand) %>%
#   group_by(black_q, white_q) %>% 
#   summarize(trump_pref_cand = mean(Trump > Clinton)) %>% 
#   ungroup()
#   
# ggplot(race_q, aes(white_q, black_q)) +
# geom_tile(aes(fill = trump_pref_cand)) +
# scale_fill_gradient(low = "blue", high = "red")

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}

####################
# By race analysis #
####################

by_race <- votes %>% 
  group_by(pref_cand) %>% 
  summarize(
    white    = mean(White2),
    black     = mean(Black),
    asian     = mean(Asian),
    hispanic  = mean(Hispanic),
    foreign  = mean(Foreign)) %>% 
  # reshape to ggplot
  melt(id.vars = 'pref_cand')

ggplot(by_race) + 
  geom_bar(aes(x = variable, y = value, fill = pref_cand),
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

ggplot(by_race, aes(x = pref_cand, y = value)) + 
  geom_col(aes(fill = pref_cand)) + 
  facet_wrap(~variable
             # , scales = "free_y"             
  ) + 
  scale_fill_manual(values = alpha(c("blue", "red"))) 

#######################
# Black race analysis #
#######################

votes %>% 
  group_by(pref_cand, black_q) %>% 
  summarize(mean_black = mean(Black)) %>% 
  ggplot() + 
  geom_bar(aes(x = black_q, y = mean_black, fill = pref_cand),
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
  group_by(pref_cand, white_q) %>% 
  summarize(mean_white = mean(White2)) %>% 
  ggplot() + 
  geom_bar(aes(x = white_q, y = mean_white, fill = pref_cand),
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
# Assign 1 to Trump winning
  mutate(pref_cand = ifelse(pref_cand == "T", 1, 0)) %>% 
  nest(-urban_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand ~ Black, ., 
                         family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "Black") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(urban_q)

votes %>% 
  # Assign 1 to Trump winning
  mutate(pref_cand = ifelse(pref_cand == "T", 1, 0)) %>% 
  nest(-Income_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand ~ Black, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "Black") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(Income_q)
# 
# votes %>%
#   # Assign 1 to Trump winning
#   mutate(pref_cand = ifelse(pref_cand == "T", 1, 0)) %>%
#   nest(-Edu_batch_q) %>%
#   mutate(models = map(data, ~ glm(pref_cand ~ Black, .,
#                                   family = "binomial"))) %>%
#   unnest(map(models, tidy)) %>%
#   filter(term == "Black") %>%
#   filter(p.adjust(p.value) < .05) %>%
#   arrange(Edu_batch_q)

votes %>% 
  # Assign 1 to Trump winning
  mutate(pref_cand = ifelse(pref_cand == "T", 1, 0)) %>% 
  nest(-Income_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand ~ White2, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "White2") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(Income_q)