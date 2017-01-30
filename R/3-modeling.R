library(caret)
library(rpart)
library(rpart.plot)
library(tibble)

# sampling

perc_train <- 0.7
set.seed(3333)

trainIndex <- createDataPartition(y = votes$pref_cand_T, p = perc_train, 
                                  list = FALSE, times = 1)

train <- votes %>% subset(ID %in% trainIndex)
test <-  votes %>% setdiff(train)

# modeling

pref_cand_rpart <- rpart(pref_cand_T ~ ., 
                         data = train[, -c(1:24, 70)], 
                         control = rpart.control(xval = 10, cp = 0.0001))

rpart.plot(pref_cand_rpart, main = "Winner candidate in county without prunning", 
           extra = 104, split.suffix = "?", branch = 1, 
           fallen.leaves = FALSE, box.palette = "BuRd",
           branch.lty = 3, split.cex = 1.2,
           shadow.col = "gray", shadow.offset = 0.2)

# prunning

cp <- as_tibble(pref_cand_rpart$cptable) %>%
  filter(xerror <= min(xerror) + xstd) %>% 
  filter(xerror == max(xerror)) %>% 
  select(CP) %>% 
  unlist()

winner_rpart <- prune(pref_cand_rpart, cp = cp)

# plot tree

rpart.plot(winner_rpart, main = "Winner candidate in county", 
           extra = 104, split.suffix = "%?", branch = 1, 
           fallen.leaves = FALSE, box.palette = "BuRd",
           branch.lty = 3, split.cex = 1.2,
           shadow.col = "gray", shadow.offset = 0.2)