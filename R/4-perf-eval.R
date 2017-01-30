
# Missclassification Error

test <- test %>% 
  mutate(pred = predict(winner_rpart, type = "class", test),
         pred_prob_T = predict(winner_rpart, type = "prob", test)[,2],
         error = ifelse(pred != pref_cand_T, 1, 0))

test %>% 
  summarize(missc_error = mean(error))  

# Kappa statistic (confusion matrix)

library(e1071)

test %>% 
  select(pred, pref_cand_T) %>% 
  table() %>% 
  confusionMatrix() 

# ROC Curve and AUROC

library(plotROC)

roc <- test %>% 
  select(pref_cand_T, pred_prob_T) %>% 
  mutate(pref_cand_T = as.numeric(pref_cand_T) - 1,
         pref_cand_T.str = c("Clinton", "Trump")[pref_cand_T + 1]) %>% 
  ggplot(aes(d = pref_cand_T, m = pred_prob_T)) +
  geom_roc(labels = FALSE)

roc +
  style_roc(theme = theme_bw, xlab = "False Positive Rate", ylab = "True Positive Rate") +
  theme(panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.line = element_line(colour = "grey")) +
  ggtitle("ROC Curve for winner_rpart classifier") +
  annotate("text", x = .75, y = .25,
           label = paste("AUROC =", round(calc_auc(roc)$AUC, 2))) 

#Warning about Classification Trees instability
 
set.seed(4444)

trainIndex_2 <- createDataPartition(y = votes$pref_cand_T, p = perc_train,
                                    list = FALSE, times = 1)
                              
train_2 <- votes %>% subset(ID %in% trainIndex_2)
test_2 <-  votes %>% setdiff(train_2)

winner_rpart_2 <- rpart(pref_cand_T ~ .,
                        data = train_2[, -c(1:24, 70)], 
                        control = rpart.control(xval = 10, cp = cp))
                  
rpart.plot(winner_rpart_2, main = "Winner candidate in county",
           extra = 104, split.suffix = "%?", branch = 1,
           fallen.leaves = FALSE, box.palette = "BuRd",
           branch.lty = 3, split.cex = 1.2,
           shadow.col = "gray", shadow.offset = 0.2)
                                                                                                             
# K-fold Cross Validation for model accuracy estimation

tc <- trainControl("cv", number = 10)

rpart.grid <- expand.grid(cp = cp)

train.rpart <- train(pref_cand_T ~ .,
                     data = votes[,-c(1:24, 70)], method = "rpart",
                     trControl = tc,  tuneGrid = rpart.grid)

train.rpart$results

