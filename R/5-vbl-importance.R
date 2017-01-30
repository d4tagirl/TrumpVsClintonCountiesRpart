
winner_rpart$variable.importance %>%
  data_frame(variable = names(.), importance = .) %>%
  mutate(importance = importance / sum(importance)) %>%
  top_n(20) %>%
  ggplot(aes(x = importance,
             y = reorder(variable, importance))) +
  geom_point() +
  labs(title = "Importance of county characteristics \n in determining the most voted candidate",
       subtitle = "(20 most relevant, Rpart, scaled to sum 1)") +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.line = element_line(colour = "grey"),
        panel.grid.major = element_blank(), panel.border = element_blank()) +
  geom_segment(aes(x = -Inf, y = reorder(variable, importance), 
                   xend = importance, yend = reorder(variable, importance)),
               size = 0.2)

save.image("trump-rpart.Rdata")