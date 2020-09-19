###############
### Protein ###
###############

### Loading in Libraries
library(tidyverse)
library(DataExplorer)
library(caret)

### Reading in Data
protein.all <- bind_rows(read_csv("train.csv"), read_csv("test.csv"))

plot_missing(protein.all)

sum(is.na(protein.all$Consensus) == is.na(protein.all$PSSM))
View(protein.all[is.na(protein.all$Consensus),c("Consensus", "PSSM")])


corrgram::corrgram(protein.all[!is.na(protein.all$Consensus),])

# EDA
GGally::ggpairs(protein.all[!is.na(protein.all$Consensus),-9])

protein.all[!is.na(protein.all$Response),-9] %>% ggplot() + geom_density(aes(x = PSSM, color = as.factor(Response)))

# Linear Regression Imputation for Consensus
protein.consensus.lm <- lm(Consensus ~ SVM, data = protein.all)
protein.all$Consensus[is.na(protein.all$Consensus)] <- predict.lm(protein.consensus.lm, newdata = protein.all[is.na(protein.all$Consensus),])

# Linear Regression Imputation for
protein.pssm.lm <- lm(PSSM ~ SVM + ANN + Consensus, data = protein.all)
protein.all$PSSM[is.na(protein.all$PSSM)] <- predict.lm(protein.pssm.lm, newdata = protein.all[is.na(protein.all$PSSM),])

plot_missing(protein.all)

### Training Model ###

library(e1071)
library(ranger)
library(dplyr)


train.model <- train(form = as.factor(Response)~., 
                     data = protein.all %>% filter(!is.na(Response)),
                     method = 'ranger',
                     trControl = trainControl(method = 'repeatedcv',
                                              number = 10,
                                              repeats = 3)
                     )

plot(train.model)

boost.preds <- data.frame(Id = protein.all %>% filter(is.na(Response)) %>% select(SiteNum) %>% pull, 
                          Predicted=predict(train.model, newdata=dplyr::filter(protein.all,is.na(Response))))
boost.preds$Predicted <- ifelse(boost.preds$Predicted == 1, TRUE, FALSE)
write_csv(x=boost.preds, path="./TFBoostPredictionsBenAnderson.csv")
