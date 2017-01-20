rm(list = ls()) 
load("to_publish.Rdata")

library(rpart)
library(caret)

pref_cand_rpart <- rpart(pref_cand ~ ., 
                         data = train[,-c(1:24, 31)], 
                         control = rpart.control(
                                                 xval = 15, 
                                                 cp = 0.001))

plot(pref_cand_rpart,margin=0.1,uniform=T)
text(pref_cand_rpart,all=T,use.n=T)
# 
# summary(pref_cand_rpart)
# pref_cand_rpart
# 

# Poda

plotcp(pref_cand_rpart)
print(pref_cand_rpart$cptable)

# Prunning the tree using min cv error

xerror <- pref_cand_rpart$cptable[,"xerror"] 
opt <- which.min(xerror)
cp <- pref_cand_rpart$cptable[opt, "CP"]
pref_cand_prune_minxerr <- prune(pref_cand_rpart, cp = cp)

plot(pref_cand_prune_minxerr,margin=0.1,uniform=T)
text(pref_cand_prune_minxerr,all=T,use.n=T)

# Prunning the tree using 1-se rule

minxerror <- pref_cand_rpart$cptable[opt,"xerror"]
xstd <- pref_cand_rpart$cptable[opt,"xstd"]
splt <- min(seq(along = xerror)[xerror <= minxerror +  xstd]) 

cp2 <- pref_cand_rpart$cptable[splt, "CP"]
pref_cand_prune_1se <- prune(pref_cand_rpart, cp = cp2)

plot(pref_cand_prune_1se,margin=0.1,uniform=T)
text(pref_cand_prune_1se,all=T,use.n=T)














library(partykit)
plot(as.party(pref_cand_prune_1se), type="simple")


library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(pref_cand_prune_1se)

# Evaluacion del ajuste sobre datos de entrenamiento
# Error de clasificación

pred.cart.train = predict(pref_cand_prune_1se, type="class", train)
addmargins(table(pred.cart.train, train$pref_cand))

mean(pred.cart.train != train$pref_cand)

model <- train(pref_cand ~ ., 
               data = train[, -c(1:24, 31)], 
               method='rpart', 
               tuneLength=10,
               trControl=trainControl(
                 method='cv', 
                 number=10, 
                 classProbs=TRUE, 
                 summaryFunction=twoClassSummary))
model
plot(model)