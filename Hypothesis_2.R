library(randomForest)
library(caret)
library(e1071)
library(nnet)
# Lets us try to find which variables were important for the prediction of sector
# Variable importance~sector-Hypothesis 2
# Random forest
rfModel_sector <- randomForest(sector ~ .-region-date, data = df_asia_2017_New, ntree = 500, importance = TRUE, 
                               mtry = 6)
rfModel_sector
importance(rfModel_sector)
varImpPlot(rfModel_sector)

# Splitting the data(Creating train and test data set)
set.seed(123)
TRG_PCT = 0.7
nr1 = nrow(df_asia_2017_New)
trnIndex = sample(1:nr1, size = round(TRG_PCT*nr1), replace=FALSE) #get a random 70%sample of row-indices
kiva_loans_trn = df_asia_2017_New[trnIndex,]   #training data with the randomly selected row-indices
kiva_loans_tst = df_asia_2017_New[-trnIndex,]  #test data with the other row-indices

# Let us begin the MODELLING process
# Decision tree

# Random forest-(also called BAGGING)
Model_BAGGING <- randomForest(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                                        country+borrower_genders+intensity_Depri_rural,data = kiva_loans_trn, 
                                        ntree = 500, importance = TRUE, mtry = 8)
predictBTst<-predict(Model_BAGGING,newdata = kiva_loans_tst)
confusionMatrix(predictBTst,kiva_loans_tst$sector,mode = "prec_recall")
# Accuracy : 0.5169
# Random forest
Model_RANDOMFOREST <- randomForest(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                                              country+borrower_genders+intensity_Depri_rural,data = kiva_loans_trn, 
                                              ntree = 500, importance = TRUE, mtry = 3)
predictRFTst<-predict(Model_RANDOMFOREST,newdata = kiva_loans_tst)
confusionMatrix(predictRFTst,kiva_loans_tst$sector,mode = "prec_recall")
# Accuracy : 0.5404  

# Lets us try using other technique
# Multinomial Logistic Regression
multinomModel <- multinom(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                            country+borrower_genders+intensity_Depri_rural, data=kiva_loans_trn) # multinom Model
predictMLTst<-predict(multinomModel,newdata = kiva_loans_tst)
confusionMatrix(predictMLTst,kiva_loans_tst$sector,mode = "prec_recall")
# Accuracy : 0.5216   
#####################################################################################################################

set.seed(123)
install.packages('rpart')
library(rpart)
rpModel1=rpart(sector ~ loan_theme_type+month+loan_amount+term_in_months+repayment_interval+
                 country+borrower_genders+intensity_Depri_rural, data=kiva_loans_trn, method="class", parms = list(split ='gini'))
predTrn=predict(rpModel1, gcData, type='class')
table(pred = predTrn, true=gcData$RESPONSE)
#    true
#pred   0   1
#   0 181  98
#   1 119 602
#Accuracy on bad cases : 60.3%
#Accuracy on good cases: 86.0%
#Model Accuracy
mean(predTrn==gcData$RESPONSE)
#78.3%

index<-sample(1:41262,100,replace = FALSE)
kiva_loans_trn_sample<-kiva_loans_trn[index,]


install.packages('randomForest')
library(randomForest)
rfModel <- randomForest(sector ~ .-region-date,
                        data = df_asia_2017_New, ntree = 500, importance = TRUE, mtry = 6)
rfModel
importance(rfModel)
varImpPlot(rfModel)

###########################################################################################
dt<-df_asia_2017_New[df_asia_2017_New$sector,c(12,8,18,3,4)]
x<- 60/100
rid<-sample(1:nrow(dt),x*nrow(dt))
train<-dt[rid,]
test<-dt[-rid,]

start.time <- Sys.time()

dt_model<-rpart(sector~.,train,method = "class")
prp(dt_model)


###########################################################################################
dt<-df_asia_2017_New[df_asia_2017_New$sector,c(12,8,18,3,4)]
x<- 60/100

corte <- sample(nrow(dt),nrow(dt)*x)
train<-dt[corte,]
test<-dt[-corte,]

start.time <- Sys.time()
install.packages('naivebayes')
library(naivebayes)
modelo<-naive_bayes(sector~.,data=train)
predBayes<-predict(modelo, newdata = test)
test$prediction<-predBayes
summary(predBayes)
end.time <- Sys.time()
btime <- end.time - start.time

kable(head(test,10), "html") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16)

###########################################################################################
d <- expand.grid(id = 1:35000, stratum = letters[1:10])
p = 0.1
dsample <- data.frame()
for(i in levels(d$stratum)) {
  dsub <- subset(d, d$stratum == i)
  B = ceiling(nrow(dsub) * p)
  dsub <- dsub[sample(1:nrow(dsub), B), ]
  dsample <- rbind(dsample, dsub)   
}
#############################################################################################
set.seed(123)
train <- df_asia_2017 %>%
  distinct %>%
  group_by(sector) %>%
  sample_frac(0.6) %>%
  left_join(df_asia_2017)

test <- df_asia_2017[!(df_asia_2017$id %in% train$id), ]

train<-train[,c(3,4,5,7,8,9,13,14,15,16,17,18)]
test<-test[,c(3,4,5,7,8,9,13,14,15,16,17,18)]

modelo<-naiveBayes(sector~.,data=train)
predBayes<-predict(modelo, newdata = test)
test$prediction<-predBayes

summary(modelo)
print(modelo)
summary(predBayes)
table(train$sector)
table(test$sector)



cfm<-confusionMatrix(predBayes,test$sector)
bacc<-cfm$overall[1]

kable(cfm$overall, "html", col.names = "") %>%
  kable_styling(bootstrap_options = "striped", full_width = F, font_size = 16, colnames(""))