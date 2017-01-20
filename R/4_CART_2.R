rm(list = ls()) 
load("base.Rdata")

library(rpart)
library(caret)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Trees without priors
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

pref_cand_rpart <- rpart(pref_cand_T ~ ., 
                         data = train[,-c(1:24, 70)], 
                         control = rpart.control(xval = 10, 
                                                 cp = 0.001)
                         )

xerror     <- pref_cand_rpart$cptable[,"xerror"]
opt        <- which.min(xerror)
minxerror  <- pref_cand_rpart$cptable[opt, "xerror"]
xstd       <- pref_cand_rpart$cptable[opt,"xstd"]
splt       <- min(seq(along = xerror)[xerror <= minxerror +  xstd]) 

cp                   <- pref_cand_rpart$cptable[splt, "CP"]
pref_cand_prune_1se  <- prune(pref_cand_rpart, cp = cp) 

mean(predict(pref_cand_prune_1se, test, type = "class") != test$pref_cand_T)

plot(pref_cand_prune_1se, margin = 0.1, uniform = T)
text(pref_cand_prune_1se, all = T, use.n = T)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
# Trees with priors
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

priors <- seq(0.05, 0.95, 0.05)

col_lab <- c("number of tree", "prior_0", "prior_1,", "mean_error")
row_lab <- c("0.05, 0.95", "0.1, 0.9", "0.15, 0.85", "0.2, 0.8", "0.25, 0.75", "0.3, 0.7", "0.35, 0.65", 
             "0.4, 0.6", "0.45, 0.55", "0.5, 0.5", "0.55, 0.45", "0.6, 0.4", "0.65, 0.35", 
             "0.7, 0.3", "0.75, 0.25", "0.8, 0.2", "0.85, 0.15", "0.9, 0.1", "0.95, 0.05")

info.tree = matrix(NA, nrow = length(priors), ncol = 4, dimnames = list(row_lab, col_lab))
trees <- list()

for (i in 1:length(priors)) {
  p <- c(priors[i], 1 - priors[i])
  pref_cand_rpart_priors <- rpart(pref_cand_T ~ ., 
                              data = train[,-c(1:24, 70)], 
                              parms = list(prior = p),
                              control = rpart.control(xval = 10, 
                                                      cp = 0.001)
  )
  
  xerror     <- pref_cand_rpart_priors$cptable[,"xerror"] 
  opt        <- which.min(xerror)
  minxerror  <- pref_cand_rpart_priors$cptable[opt, "xerror"]
  xstd       <- pref_cand_rpart_priors$cptable[opt,"xstd"]
  splt       <- min(seq(along = xerror)[xerror <= minxerror +  xstd]) 
  
  cp         <- pref_cand_rpart_priors$cptable[splt, "CP"]
  pref_cand_priors_prune_1se  <- prune(pref_cand_rpart_priors, cp = cp) 
  
  trees[[i]] <- pref_cand_priors_prune_1se
  
  info.tree[i,1] <- i
  info.tree[i,2] <- priors[i]
  info.tree[i,3] <- 1 - priors[i]
  info.tree[i,4] <- mean(predict(pref_cand_priors_prune_1se, test, type = "class") != test$pref_cand_T)
}

info.tree

save.image("tree.Rdata")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# plot tree

rpart.plot(pref_cand_prune_1se,
           extra = 104, 
           box.palette = "BuRd",
           branch.lty = 3, 
           shadow.col = "gray", 
           nn = TRUE)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# Training error rate

pred.cart.train = predict(pref_cand_prune_1se, type = "class", train)
addmargins(table(pred.cart.train, train$pref_cand))

mean(pred.cart.train != train$pref_cand)

# Evaluacion del ajuste sobre datos de test - 30-fold Cross Validation

K = 30	
error.kfold = list(matrix(NA, K))
set.seed(1)
for (k in 1:K)	{
  Index <- createDataPartition(y = votes$pref_cand_T, 
                               p = perc_train, 
                               list = FALSE,
                               times = 1)
  train <- votes %>% subset(ID %in% Index)
  test <-  votes %>% setdiff(train)
  model.kfold = rpart(pref_cand_T ~ ., 
                      data = train[,-c(1:24, 70)], 
                      control = rpart.control(xval = 10, 
                                               cp = cp))
  pred.kfold.test = predict(model.kfold, 
                            type = "class", 
                            test[, -c(1:24, 70)])
  error.kfold[k] = mean(pred.kfold.test != test$pref_cand_T)
}				

(mean.error.kfold = mean(unlist(error.kfold))) # [1] 0.09516924
(sd.error.kfold = sd(unlist(error.kfold))) # [1] 0.01230117

# - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# importance of variables
library(tibble)

importance <- pref_cand_prune_1se$variable.importance %>% 
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
        plot.subtitle = element_text(hjust = 0.5))
