#Alzheimer Analysis and Prediction

######################Elder Research Project####################

#Packaging libraries into my_libraries
my_libraries <- c("ggplot2", "dplyr", "Hmisc", "PerformanceAnalytics", "cowplot", "caret", "rpart","rpart.plot", "e1071", "randomForest","gbm", "Metrics","randomForest", "vtreat", "e1071", "AUC", "rgl", "reshape2","fpc", "ROCR","Rglpk")


#Load the libraries
lapply(my_libraries, library, character.only=TRUE)

set.seed(123)
Data <- read.csv("all_adni_data.csv")
print(sample_n(Data, 5))
describe(Data)

Data[is.na(Data)] <- 0

#Distribution of AGE by CDR_GLOBAL 
Data %>%
  select(RID, AGE, dxsum_DIAGNOSIS, Male) %>%
  group_by(RID, dxsum_DIAGNOSIS, Male) %>%
  summarise_all(funs(min)) %>%
  as.data.frame() %>%
  mutate(dxsum_DIAGNOSIS = as.factor(dxsum_DIAGNOSIS)) %>%
  ggplot(aes(x = dxsum_DIAGNOSIS, y = AGE, fill = Male)) + 
  geom_violin() +
  labs(title = "1. Distribution of Age by Dimentia rate",
       fill = "Sex") +
  theme_light()

#Distribution of education and social economic status

x <- Data %>%
  select(mmse_MMSCORE, dxsum_DIAGNOSIS, Male) %>%
  mutate(dxsum_DIAGNOSIS = as.factor(dxsum_DIAGNOSIS)) %>%
  ggplot(aes(x = dxsum_DIAGNOSIS, y = mmse_MMSCORE)) + 
  geom_jitter(aes(col = dxsum_DIAGNOSIS), alpha = 0.6) +
  labs(title = "x") +
  theme_light()

y <- Data %>%
  select(ucsff_Hippocampus_volume, dxsum_DIAGNOSIS, Male) %>%
  mutate(dxsum_DIAGNOSIS = as.factor(dxsum_DIAGNOSIS)) %>%
  ggplot(aes(x = dxsum_DIAGNOSIS, y = ucsff_Hippocampus_volume)) + 
  geom_jitter(aes(dxsum_DIAGNOSIS), alpha = 0.6) +
  labs(title = "x") +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Distribution of MMSE Score and Wole-brain Volume", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

# Distribution of ecog_VISSPAT1  npiq_NPIB
x <- Data %>%
  select(ecog_VISSPAT1, dxsum_DIAGNOSIS, Male) %>%
  mutate(dxsum_DIAGNOSIS = as.factor(dxsum_DIAGNOSIS)) %>%
  ggplot(aes(x = dxsum_DIAGNOSIS, y = ecog_VISSPAT1)) + 
  geom_jitter(aes(col = dxsum_DIAGNOSIS), alpha = 0.6) +
  labs(title = "x") +
  theme_light()

y <- Data %>%
  select(npiq_NPIB, dxsum_DIAGNOSIS, Male) %>%
  mutate(dxsum_DIAGNOSIS = as.factor(dxsum_DIAGNOSIS)) %>%
  ggplot(aes(x = dxsum_DIAGNOSIS, y = npiq_NPIB)) + 
  geom_jitter(aes(col = dxsum_DIAGNOSIS), alpha = 0.6) +
  labs(title = "x") +
  theme_light()

p <- plot_grid(x, y) 
title <- ggdraw() + draw_label("Distribution of ecog_VISSPAT1 and npiq_NPIB", fontface='bold')
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

#prepairing data
Data_new <- Data %>%
  select(AGE, Male, Married, adas_TOTSCORE, adas_Q4SCORE, adas_Q1SCORE, adas_Q8SCORE, ecog_mean, ecog_ORGAN5, ecog_DIVATT4, ecog_PLAN4, ecog_MEMORY6, ecog_MEMORY1, ecog_MEMORY3, faq_FAQTOTAL, faq_FAQFORM,fhq_FHQDAD,fhq_FHQMOMAD,gdscale_GDSATIS,gdscale_GDSPIRIT, npiq_NPIK, npiq_NPIJ, npiq_NPII, npiq_NPIH, npiq_NPIG, npiq_NPIF, npiq_NPIF, npiq_NPIE, npiq_NPID, npiq_NPIC, npiq_NPIB, npiq_NPIA, neurobat_RAVLT_FORGETTING, neurobat_RAVLT_LEARNING, neurobat_DSPANFLTH, neurobat_TRAASCOR,PTEDUCAT, mmse_MMSCORE, loclab_CTRED, loclab_CTWHITE, loclab_GLUCOSE, loclab_PROTEIN, gdscale_GDTOTAL, modhach_HMSCORE, ucsff_Hippocampus_volume, cci_CCI12TOT, Male, apoe_APOE4, upennplasma_AB40, upennplasma_AB42, upennplasma_AB42_40_ratio, blennowplasmatau_PLASMATAU,	ucberkeleyfdg_Mean_Overall, vitals_VSBPSYS, vitals_VSBPDIA, vitals_VSPULSE,	vitals_VSRESP, vitals_VSTEMP, dxsum_DIAGNOSIS) %>%
  mutate(dxsum_DIAGNOSIS  = as.factor(dxsum_DIAGNOSIS))

n_train <- round(0.8 * nrow(Data_new)) #80% of length of main data set as integer
train_indices <- sample(1:nrow(Data_new), n_train) #creating a vector with random indices
train <- Data_new[train_indices, ] #generating train data set
test <- Data_new[-train_indices, ] #generating test data set

formula <- dxsum_DIAGNOSIS ~ AGE + Male + Married + adas_TOTSCORE + adas_Q4SCORE + adas_Q1SCORE + adas_Q8SCORE+ ecog_mean+ ecog_ORGAN5+ ecog_DIVATT4+ ecog_PLAN4+ ecog_MEMORY6+ ecog_MEMORY1+ ecog_MEMORY3+ faq_FAQTOTAL+ faq_FAQFORM+ fhq_FHQDAD+ fhq_FHQMOMAD+ gdscale_GDSATIS+ gdscale_GDSPIRIT+ npiq_NPIK+ npiq_NPIJ+ npiq_NPII+ npiq_NPIH+ npiq_NPIG+ npiq_NPIF+ npiq_NPIF+ npiq_NPIE+ npiq_NPID+ neurobat_RAVLT_FORGETTING+ neurobat_RAVLT_LEARNING+ neurobat_DSPANFLTH+ neurobat_TRAASCOR+ PTEDUCAT+ mmse_MMSCORE+ loclab_CTRED+ loclab_CTWHITE+ loclab_GLUCOSE+ loclab_PROTEIN+ gdscale_GDTOTAL+ modhach_HMSCORE+ ucsff_Hippocampus_volume+  cci_CCI12TOT+  apoe_APOE4+ upennplasma_AB40+ upennplasma_AB42+ upennplasma_AB42_40_ratio+ blennowplasmatau_PLASMATAU+ ucberkeleyfdg_Mean_Overall+  vitals_VSBPSYS+ vitals_VSBPDIA+ vitals_VSPULSE+ vitals_VSRESP+  vitals_VSTEMP 

# 5-folds cross validation plan
k <- 5
splitPlan <- kWayCrossValidation(nrow(Data_new), k, NULL, NULL) 


#Decision Tree Model
opt_cp <- 0 #list with optimal CP parameters
for(i in 1:k) {
  split <- splitPlan[[i]]
  #training simple decision tree model
  model_cv <- rpart(formula = formula,
                    data = Data_new[split$train,],
                    method = "class")
  #get the best CP value
  opt_cp[i] <- model_cv$cptable[which.min(model_cv$cptable[,"xerror"]),"CP"]
}

#training the model with optimal CP parameter on whole data set
model_dt <- rpart(formula = formula,
                  data = Data_new,
                  method = "class",
                  cp = mean(opt_cp))

#plot decision tree model
prp(x = model_dt, type=1, extra = 102)


#testing the model
prediction_dt <- predict(object = model_cv,
                         newdata = Data_new,
                         type = "class")

#print confusion matrix
confusionMatrix(data = prediction_dt,
                reference = Data_new$dxsum_DIAGNOSIS)

AUC_dt <- Metrics::auc(actual = Data_new$dxsum_DIAGNOSIS, predicted = prediction_dt) #calculating AUC

#cross-validation
prediction_dt_cv <- 0
for(i in 1:k) {
  split <- splitPlan[[i]]
  #training decision tree model
  model_cv <- rpart(formula = formula,
                    data = Data_new[split$train,],
                    method = "class",
                    cp = mean(opt_cp))
  #testing the model
  prediction_dt_cv[split$app] <- predict(object = model_cv,
                                         newdata = Data_new[split$app,],
                                         type = "class")
}

#create function which returns vector in original scale
conv_to_orig <- function(x){
  x[x == 1] <- 0
  x[x == 2] <- 0.5
  x[x == 3] <- 1
  x[x == 4] <- 2
  x <- as.factor(x)
  return(x)
}

prediction_dt_cv <- conv_to_orig(prediction_dt_cv)


AUC_dt_cv <- Metrics::auc(actual = Data_new$cdr_CDJUDGE, predicted = prediction_dt_cv)

print(paste0("AUC of the full model's predictions = ", round(AUC_dt, 3)))


#training with random forest model
model_rf0 <- randomForest(formula = formula,
                          data = train,
                          importance=TRUE)

# Print the model output                             
print(model_rf0)


plot(model_rf0, main = "Model Error by Number of Trees")
legend(x = "right", 
       legend = colnames(model_rf0$err.rate),
       fill = 1:ncol(model_rf0$err.rate))

#plot variance importance
varImpPlot(model_rf0, main = "Importance of Variables") 


#possible values for mtry, nodesize and sampsize
mtry <- seq(4, ncol(train), 2)
nodesize <- seq(3, 8, 2)
sampsize <- as.integer(nrow(train) * c(0.7, 0.8, 0.9))

#creation of data frame containing all combinations 
hyper_grid <- expand.grid(mtry = mtry, nodesize = nodesize, sampsize = sampsize) 

oob_err <- c() #empty vector to store OOB error values

#Loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  #train a Random Forest model
  model_rf <- randomForest(formula = formula,
                           data = train,
                           mtry = hyper_grid$mtry[i],
                           nodesize = hyper_grid$nodesize[i],
                           sampsize = hyper_grid$sampsize[i])
  
  #store OOB error for the model                      
  oob_err[i] <- model_rf$err.rate[nrow(model_rf$err.rate), "OOB"]
}

#identify optimal set of hyperparmeters based on OOB error
opt_i <- which.min(oob_err)

#train a final Random Forest model with new parameters
model_rf_final <- randomForest(formula = formula,
                               data = train,
                               mtry = hyper_grid$mtry[opt_i],
                               nodesize = hyper_grid$nodesize[opt_i],
                               sampsize = hyper_grid$sampsize[opt_i])

prediction_rf <- predict(object = model_rf_final,
                         newdata = select(test, -dxsum_DIAGNOSIS),
                         type = "class")

confusionMatrix(data = prediction_rf, reference = test$dxsum_DIAGNOSIS) 

AUC_rf <- Metrics::auc(actual = test$dxsum_DIAGNOSIS, predicted = prediction_rf)

print(paste0("AUC of the full model's predictions = ", round(AUC_rf, 3)))

# Train a 5000-tree GBM model
model_gbm <- gbm.fit(x = select(train, -dxsum_DIAGNOSIS),
                     y = train$dxsum_DIAGNOSIS,
                     distribution = "multinomial", 
                     n.trees = 5000,
                     shrinkage = 0.01,
                     nTrain = round(nrow(train) * 0.8),
                     verbose = FALSE)

                    
# Print the model object                    
print(model_gbm)

# summary() prints variable importance
summary(model_gbm)

prediction_gbm <- predict.gbm(object = model_gbm, 
                              newdata = select(test, -dxsum_DIAGNOSIS),
                              type = "response",
                              n.trees = gbm.perf(model_gbm, plot.it = FALSE))
## Using test method.
prediction_gbm <- apply(prediction_gbm, 1, which.max)
prediction_gbm <- conv_to_orig(prediction_gbm)


AUC_gbm <- Metrics::auc(actual = test$dxsum_DIAGNOSIS, predicted = prediction_gbm)


roc1 = AUC::roc(prediction_dt_cv, Data_new$dxsum_DIAGNOSIS)
roc2 = AUC::roc(prediction_gbm, test$dxsum_DIAGNOSIS)
roc3 = AUC::roc(prediction_rf, test$dxsum_DIAGNOSIS)
plot(roc1, col = 1, lty = 2, main = "ROC")
plot(roc2, col = 3, lty = 4, add = TRUE)
plot(roc3, col = 4, lty = 3, add = TRUE)

print(paste0("AUC for Decision Tree Model = ", round(AUC_dt, 2)))

print(paste0("AUC for Random Forest Model = ", round(AUC_rf, 2)))

print(paste0("AUC for GBM Model = ", round(AUC_gbm, 2)))

legend(0.6, 0.3, legend=c("Decision Tree Model", "Random Forest Model", "GBM Model"),
       col=c(1,3,4), lty=2:4, cex=0.8)

