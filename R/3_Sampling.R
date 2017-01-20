rm(list = ls()) 
load(("base.Rdata"))

library(caret)

# Train and Test Sampling

perc_train <- 0.7
set.seed(3333)

# sampling conserving the proportion of both classes
trainIndex <- createDataPartition(y = votes$pref_cand, 
                                  p = perc_train, 
                                  list = FALSE,
                                  times = 1)

train <- votes %>% 
  subset(ID %in% trainIndex)

test <-  votes %>%
  setdiff(train)

save.image("base.Rdata")