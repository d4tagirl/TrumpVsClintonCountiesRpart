library(dplyr)
library(ggplot2)
library(purrr)
library(broom)
library(tidyr)

nt <- partial(ntile, n = 5)

votes <- votes %>% 
  # create quintile variables by race
  mutate(white_alone_q    = nt(white_alone),
         white_q          = nt(white),
         black_q          = nt(black),
         asian_q          = nt(asian),
         hisp_latin_q     = nt(hisp_latin),
         foreign_q        = nt(foreign),
  # create quintile variables by education
         edu_batch_q      = nt(edu_batchelors), 
  # create quintile variables by Housing_units_multistruct
         urban_q          = nt(housing_units_multistruct),
  # create quintile variables by Income
         income_q         = nt(income))
# 
# race_q <- votes %>%
#   select(Clinton, Trump, pref_cand_T,
#          white_alone_q, white_q,
#          black_q, asian_q,
#          hisp_latin_q, foreign_q) %>%
#   group_by(black_q, white_alone_q) %>%
#   summarize(trump_pref_cand = mean(Trump > Clinton)) %>%
#   ungroup()

# ggplot(race_q, aes(white_alone_q, black_q)) +
# geom_tile(aes(fill = trump_pref_cand)) +
# scale_fill_gradient(low = "blue", high = "red")


####################
# By race analysis #
####################

# plot by candidate
by_candidate <- votes %>% 
  group_by(pref_cand_T) %>% 
  summarize(
    white       = mean(white),
    white_alone = mean(white_alone),
    black       = mean(black),
    asian       = mean(asian),
    hisp_latin  = mean(hisp_latin),
    foreign     = mean(foreign)) %>% 
  gather(variable, value, -pref_cand_T) %>% 
  ggplot() + 
  geom_bar(aes(x = variable, y = value, fill = pref_cand_T),
           stat = 'identity', position = 'dodge') + 
  geom_vline(xintercept = c(4.5, 5.5), alpha = 0.2 ) +
  scale_fill_manual(values = alpha(c("blue", "red")), 
                    breaks = c("0", "1"), labels = c("Clinton", "Trump")) +
  scale_x_discrete(limits = limits) +
  labs(title = "Mean of % in counties",    
       subtitle = "(Simple mean of % in counties without considering counties' population)",
       fill = "winner") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.border = element_blank())

# ggplot(by_race, aes(x = pref_cand_T, y = value)) + 
# geom_col(aes(fill = pref_cand_T)) + 
# facet_wrap(~variable
#              # , scales = "free_y"
#            ) + 
# scale_fill_manual(values = alpha(c("blue", "red"))) 

#######################
# Black race analysis #
#######################

# black_q_ggplot <- votes %>% 
#   group_by(pref_cand_T, black_q) %>% 
#   summarize(mean_black = mean(black)) %>% 
#   ggplot() + 
#   geom_bar(aes(x = black_q, y = mean_black, fill = pref_cand_T),
#            stat = 'identity', 
#            position = 'dodge'
#   ) + 
#   scale_fill_manual(values = alpha(c("blue", "red"))) +
#   labs(
#     title = "Proportion of black race in counties, among counties ordered by Black race population",
#     subtitle = "(Simple mean of % in counties without considering counties' population)",
#     x = "Black Race quintile",
#     y = "Mean of % of black people") 

black_q <- votes %>% 
  group_by(black_q) %>% 
  summarize(mean_Trump = mean(pref_cand_T == 1)) %>% 
  ggplot(aes(x = black_q, y = mean_Trump)) + 
  geom_point() +
  labs(
    title = "% of counties that voted for Trump, among black quintiles",
    subtitle = "(Simple mean of % in counties without considering counties' population)",
    x = "Black quintile",
    y = "Mean of % of counties that voted for Trump") +
  ylim(0.5, 1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "grey"), legend.position = "bottom", 
        panel.border = element_blank()) +
  geom_segment(aes(x = black_q, y = -Inf, 
                   xend = black_q, yend = mean_Trump),
               size = 0.2)

#######################
# White race analysis #
#######################
# 
# white_alone_q_ggplot <- votes %>% 
#   group_by(pref_cand_T, white_alone_q) %>% 
#   summarize(mean_white_alone = mean(white_alone)) %>% 
#   ggplot() + 
#   geom_bar(aes(x = white_alone_q, y = mean_white_alone, fill = pref_cand_T),
#            stat = 'identity', 
#            position = 'dodge'
#   ) + 
#   scale_fill_manual(values = alpha(c("blue", "red"))) +
#   labs(
#     title = "Proportion of white race in counties, among counties ordered by white race population",
#     subtitle = "(Simple mean of % of counties without considering county population)",
#     x = "White Race Quintile",
#     y = "Mean of % of white people") +
#   theme_bw() +
#   theme(axis.line = element_line(colour = "grey"), legend.position = "bottom", 
#         panel.grid.major = element_blank(), panel.border = element_blank())

white_alone_q <- votes %>% 
  group_by(white_alone_q) %>% 
  summarize(mean_Trump = mean(pref_cand_T == 1)) %>% 
  ggplot(aes(x = white_alone_q, y = mean_Trump)) + 
  geom_point() +
  labs(
    title = "% of counties that voted for Trump, among white_only quintiles",
    subtitle = "(Simple mean of % in counties without considering counties' population)",
    x = "White_alone quintile",
    y = "Mean of % of counties that voted for Trump") +
  ylim(0.5, 1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "grey"), legend.position = "bottom", 
        panel.border = element_blank()) +
  geom_segment(aes(x = white_alone_q, y = -Inf, 
                   xend = white_alone_q, yend = mean_Trump),
               size = 0.2)

# plot
grid.arrange(white_alone_q, black_q, nrow = 2)

######################################
# Thinking about some correlation... 
######################################

# white_alone
# 
# votes %>% 
#   nest(-urban_q) %>% 
#   mutate(models = map(data, ~ glm(pref_cand_T ~ white_alone, ., 
#                                   family = "binomial"))) %>% 
#   unnest(map(models, tidy)) %>% 
#   filter(term == "white_alone") %>%
#   filter(p.adjust(p.value) < .05) %>% 
#   arrange(urban_q) %>% 
#   ggplot(aes(x = urban_q, y = estimate)) +
#   geom_point()

votes %>% 
  nest(-income_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand_T ~ white_alone, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "white_alone") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(income_q) %>% 
  ggplot(aes(x = income_q, y = estimate)) +
  geom_point()
# 
# votes %>%
#   nest(-edu_batch_q) %>%
#   mutate(models = map(data, ~ glm(pref_cand_T ~ white_alone, .,
#                                   family = "binomial"))) %>%
#   unnest(map(models, tidy)) %>%
#   filter(term == "white_alone") %>%
#   filter(p.adjust(p.value) < .05) %>%
#   arrange(edu_batch_q) %>% 
#   ggplot(aes(x = edu_batch_q, y = estimate)) +
#   geom_point()


# black
# 
# votes %>% 
#   nest(-urban_q) %>% 
#   mutate(models = map(data, ~ glm(pref_cand_T ~ black, ., 
#                                   family = "binomial"))) %>% 
#   unnest(map(models, tidy)) %>% 
#   filter(term == "black") %>%
#   filter(p.adjust(p.value) < .05) %>% 
#   arrange(urban_q) %>% 
#   ggplot(aes(x = urban_q, y = estimate)) +
#   geom_point()

votes %>% 
  nest(-income_q) %>% 
  mutate(models = map(data, ~ glm(pref_cand_T ~ black, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "black") %>%
  filter(p.adjust(p.value) < .05) %>% 
  arrange(income_q) %>% 
  ggplot(aes(x = income_q, y = estimate)) +
  geom_point()
# 
# votes %>%
#   nest(-edu_batch_q) %>%
#   mutate(models = map(data, ~ glm(pref_cand_T ~ black, .,
#                                   family = "binomial"))) %>%
#   unnest(map(models, tidy)) %>%
#   filter(term == "black") %>%
#   filter(p.adjust(p.value) < .05) %>%
#   arrange(edu_batch_q) %>% 
#   ggplot(aes(x = edu_batch_q, y = estimate)) +
#   geom_point()

# by states, regions

library(datasets)

regions <- tibble(state.region, state.abb)

votes <- votes %>% 
  inner_join(regions, 
             by = c("state_abbr" = "state.abb"))

# study race and % votes for each candidates by_region
votes %>% 
  group_by(state.region) %>% 
  summarise(mean_white_alone = mean(white_alone),
            mean_black       = mean(black),
            mean_trump       = mean(Trump),
            mean_clinton     = mean(Clinton))

# estimate model by region

by_region_white <- votes %>% 
  nest(-state.region) %>% 
  mutate(models = map(data, ~ glm(pref_cand_T ~ white_alone, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "white_alone") %>%
  mutate(OR = exp(estimate))

# for a one-unit increase in white_alone population in the south, we expect to see about 13% increase 
# in the odds of voting for Trump.  

by_region_black <- votes %>% 
  nest(-state.region) %>% 
  mutate(models = map(data, ~ glm(pref_cand_T ~ black, ., 
                                  family = "binomial"))) %>% 
  unnest(map(models, tidy)) %>% 
  filter(term == "black") %>%
  mutate(OR = exp(estimate))

ggplot() +
  geom_point(data = by_region_black, aes(x = reorder(state.region, estimate), y = estimate), color = "red") +
  geom_point(data = by_region_white, aes(x = reorder(state.region, estimate), y = estimate), color = "blue") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.border = element_blank())
