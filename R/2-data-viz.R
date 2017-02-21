library(tidyr)
library(ggplot2)
library(gridExtra)

summary <- votes %>% summarize(Trump       = sum(pref_cand_T == 1),
                               Clinton     = n() - Trump,
                               Trump_per   = mean(pref_cand_T == 1),
                               Clinton_per = 1 - Trump_per)

knitr::ktable(summary, align = 'l')

# Arrange axis
limits = c("white", "white_alone", "black", "asian", "hisp_latin", "foreign")

# plot total
total <- votes %>% 
  summarize(
    white       = mean(white),
    white_alone = mean(white_alone),
    black       = mean(black),
    asian       = mean(asian),
    hisp_latin  = mean(hisp_latin),
    foreign     = mean(foreign)) %>%
  gather(variable, value) %>% 
  ggplot() +
  geom_bar(aes(x = variable, y = value), 
           stat = 'identity', width = .7, fill = "#C9C9C9") +
  geom_vline(xintercept = c(4.5, 5.5), alpha = 0.2 ) +
  scale_x_discrete(limits = limits) +
  labs(title = "Mean of % in counties",    
       subtitle = "(Simple mean of % in counties without considering counties' population)") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.text.x = element_blank(), axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(), panel.border = element_blank())

# plot by candidate

limits = c("white", "white_alone", "black", "asian", "hisp_latin", "foreign")

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
  labs(fill = "winner") +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "bottom", 
        panel.grid.major = element_blank(), panel.border = element_blank())

# plot
grid.arrange(total, by_candidate, nrow = 2)
