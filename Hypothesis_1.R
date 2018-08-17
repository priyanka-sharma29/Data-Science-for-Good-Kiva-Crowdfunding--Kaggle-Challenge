library(glmnet)
library(caret)
library(randomForest)
library(corrplot)
# Lets us try to find which variables were important for the prediction of loan_amount
# Variable importance~Loan_amount-Hypothesis 1
# Random forest
rfModel_loanamount <- randomForest(loan_amount ~ .-region-date,
                                   data = df_asia_2017_New, ntree = 200, importance = TRUE, mtry = 6)
rfModel_loanamount
importance(rfModel_loanamount)
varImpPlot(rfModel_loanamount)

# Let us begin the MODELLING process
# Viewing Corrplot for numerical variables
df_asia_2017_Num <- df_asia_2017_New[, c(2,3,8,13,14,15,16,17)]
cormat<-round(cor(df_asia_2017_Num),2)
corrplot(cormat, method="circle",type="upper", addCoef.col="black")
# Loan amount highly correlated with funded amount
# Good positive correlation with term_in_months and negative correlation with intensity_Depri_urban,intensity_Depri_rural
# We can also see MPI_rural negatively correlated with hdi_2014 and positively with MPI_urban, intensity_Depri_urban,intensity_Depri_rural

# Now let us try to see which levels play a major role using LASSO regresiion for variable importance
# LASSO
x=model.matrix(loan_amount~.-1,data=df_asia_2017_New) 
y=df_asia_2017_New$loan_amount
fit.lasso=glmnet(x,y,alpha=1)
dev.off()
plot(fit.lasso,xvar="lambda",label=TRUE)
coef(fit.lasso)
cv.lasso <- cv.glmnet(x,y, alpha =1)
plot(cv.lasso)
cv.lasso$lambda.min
coef(cv.lasso)
# Viewing factors levels (relevel if needed)
table(df_asia_2017_New$loan_theme_type)
df_asia_2017_New$loan_theme_type <- relevel(df_asia_2017_New$loan_theme_type,"General")
table(df_asia_2017_New$month)
df_asia_2017_New$month <- relevel(df_asia_2017_New$month,"5")

# Splitting the data(Creating train and test data set)
set.seed(123)
TRG_PCT = 0.7
nr1 = nrow(df_asia_2017_New)
trnIndex = sample(1:nr1, size = round(TRG_PCT*nr1), replace=FALSE) #get a random 70%sample of row-indices
kiva_loans_trn = df_asia_2017_New[trnIndex,]   #training data with the randomly selected row-indices
kiva_loans_tst = df_asia_2017_New[-trnIndex,]  #test data with the other row-indices

# Function RMSE to find root mean sq error
RMSE <- function(predicted, true) mean((predicted-true)^2)^.5

# Linear regression
# Model 1 (Linear Regression)
model1 <- lm(loan_amount~term_in_months, data=kiva_loans_trn)
summary(model1)
summary(model1)$adj.r.squared
# Adjusted R-squared: 25.69%

# Model 2 (Multiple Regression)
model2 <- lm(loan_amount~term_in_months+month+loan_theme_type+repayment_interval+sector+borrower_genders
                  +intensity_Depri_urban+intensity_Depri_rural, data=kiva_loans_trn)  # build the model
summary(model2)
summary(model2)$adj.r.squared
# Adjusted R-squared:  57.68% 

# Regression diangnostics
plot(predict(model2), residuals(model2)) # Model seems OK, it has pattern
par(mfrow=c(2,2)) 
plot(model2) # we have identified some outliers
plot(hatvalues(model2)) 
identify(hatvalues(model2), col="red")
dev.off()
outliers<-c(34139,22562,38207,2227,11888,2561,11534,12479,13777,17377,26768,28240,30512,30621,31882,
            33247,33697,3740,38611)
kiva_loans_trn<-kiva_loans_trn[-outliers,]

# Checking for Multicolliniearity
vif(model2) 
sqrt(vif(model2)) > 2
# All of the variables comeback FALSE, so no risk of collinearity

# Model 3 - after Removing outliers
model3 <- lm(loan_amount~term_in_months+month+loan_theme_type+repayment_interval+sector+borrower_genders
             +intensity_Depri_urban+intensity_Depri_rural, data=kiva_loans_trn)  # build the model
summary(model3)
summary(model3)$adj.r.squared
# Adjusted R-squared:  57.81% 

# Model 4 - after taking transformations
model4 <- lm(sqrt(loan_amount)~(term_in_months)+month+loan_theme_type+repayment_interval+sector+borrower_genders
             +sqrt(intensity_Depri_urban)+sqrt(intensity_Depri_rural), data=kiva_loans_trn)  # build the model
summary(model4)
summary(model4)$adj.r.squared
# Adjusted R-squared:  57.95% 

# Improvement from Base model that is model 1
summary(model4)$adj.r.squared-summary(model1)$adj.r.squared
# We can see 32.26% jump

# Best Model -Model 4 (we will use this model for prediction)
# Prediction Train 
Prediction_train <- predict(model4, kiva_loans_trn) 
# RMSE Train
RMSE(Prediction_train, kiva_loans_trn$loan_amount)
# 537.2812

# Prediction Test
Prediction_test <- predict(model4, kiva_loans_tst) 
# RMSE Test
RMSE(Prediction_test, kiva_loans_tst$loan_amount)
# 537.2822
# We get an RMSE value of 537.2812 on our training data set. We can conclude that this is on the lower end when compared
# to the range of Dependant Variable - Loan amount i.e. 0 to 2000. 
# On the test data we get an RMSE of 537.2822, which is nearly equal to the RMSE we got from Training data, hence we can
# conclude that our Best model was a good fit model to our data. 

# Now let us try to improve this prediction by using Ridge and Lasso 
# RIDGE AND LASSO
x_trn <- model.matrix(loan_amount ~ month+term_in_months+loan_theme_type+repayment_interval+borrower_genders+ISO+sector+
                        MPI_urban+MPI_rural+intensity_Depri_urban+intensity_Depri_rural+hdi_2014, -1,
                        data = kiva_loans_trn) [,-1]
y_trn <- kiva_loans_trn$loan_amount
x_tst <- model.matrix(loan_amount ~ month+term_in_months+loan_theme_type+repayment_interval+borrower_genders+ISO+sector+
                        MPI_urban+MPI_rural+intensity_Depri_urban+intensity_Depri_rural+hdi_2014, -1, 
                        data = kiva_loans_tst) [,-1]
y_tst <- kiva_loans_tst$loan_amount

# RIDGE 
set.seed(123)
fit.ridge<-glmnet(x_trn, y_trn, alpha = 0)
dim(coef(fit.ridge))
#80 100
plot(fit.ridge, xvar="lambda", label=TRUE)
cv.ridge <- cv.glmnet(x_trn, y_trn, alpha = 0)
plot(cv.ridge)
#Optimal Lambda value
opt_lambda_ridge <- cv.ridge$lambda.min
opt_lambda_ridge
#19.10236
summary(cv.ridge$glmnet.fit)
coef(cv.ridge)
#Predicted y values
y_predicted_train_ridge <- predict(fit.ridge, s = opt_lambda_ridge, newx = x_trn)
y_predicted_test_ridge <- predict(fit.ridge, s = opt_lambda_ridge, newx = x_tst)
mean((y_predicted_test_ridge-y_tst)^2)
#48132.16
sst <- sum((y_tst - mean(y_tst))^2)
sse <- sum((y_predicted_test_ridge - y_tst)^2)
# R squared
rsq <- 1 - sse / sst
rsq
#59.30%
RMSE(y_predicted_train_ridge, y_trn)
#217.1585
RMSE(y_predicted_test_ridge, y_tst)
#219.3904
# After Ridge regression we can see that our Adjusted R squared has increased by (59.30-57.95%) 1.35%. 
# In observing RMSE we see that the value of RMSE on Train set and Test set have considerably decresed from 537 to 217( On training)
# and 219( On Test set). The difference in RMSE on Training set to Test set is (219.3904-217.1585) 2.2319 which is very low.
# Hence, we can conclude that the Ridge regression prediction increased the accuracy of the model and reduced the RMSE.

# LASSO
set.seed(123)
fit.lasso<-glmnet(x_trn, y_trn, alpha = 1)
dim(coef(fit.lasso))
#80 85
plot(fit.lasso, xvar="lambda", label=TRUE)
cv.lasso <- cv.glmnet(x_trn, y_trn, alpha = 1)
plot(cv.lasso)
#Optimal Lambda value
opt_lambda_lasso <- cv.lasso$lambda.min
opt_lambda_lasso
#0.08463532
summary(cv.lasso$glmnet.fit)
coef(cv.lasso)
#Predicted y values
y_predicted_train_lasso <- predict(fit.lasso, s = opt_lambda_lasso, newx = x_trn)
y_predicted_test_lasso <- predict(fit.lasso, s = opt_lambda_lasso, newx = x_tst)
mean((y_predicted_test_lasso-y_tst)^2)
#47436.1
sst <- sum((y_tst - mean(y_tst))^2)
sse <- sum((y_predicted_test_lasso - y_tst)^2)
# R squared
rsq <- 1 - sse / sst
rsq
#59.89%
RMSE(y_predicted_train_lasso, y_trn)
#215.1175
RMSE(y_predicted_test_lasso, y_tst)
#217.7983
# From Lasso regression coefficients list we see that for some intercepts the coefficient values are missing (or 0).
# The missing coefficient values refer to those variables who have little or no importance in the model.
# Like hdi_2014, MPI_rural, MPI_urban, also some levels of factor variables like sectorEntertainment, sectorArts, etc.
# Lasso regression increases the R squared to 59.89% and decreases the RMSE value of Training set to 215.1175 and Test set 
# to 217.7983
