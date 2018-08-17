# Kiva-Crowdfunding-Kaggle-Challenge

## Objective
For locations where Kiva has active loans, the objective is to link Kiva’s data with additional data sources to estimate loan amount and the sector based on demographic characteristics.


## Hypotheses
Hypothesis 1 - The MPI, regions, loan theme type, term in months, sector, gender may play a direct role in predicting the loan amount.

Hypothesis 2 -The loan amount, repayment interval, region, loan theme type, term in months and the MPI may play a direct role in predicting the sector in which the loans were issued.

## Exploratory Data Analysis
Initial EDA is conducted to find correlations and trends in the dataset.

## Merging Dataset
The dataset is merged with some external files like MPI(rural,urban), Human Development Index(HDI) and Continents in order to better estimate loan amount and sector.

## Data Modeling & Results
For hypothesis 1, multiple and regularized regression techniques are used for predicting the loan amount. LASSO regression was found to be the best tailored statistical model. Cross validation is used to find the penalty factor. Non-linear regression techniques can be explored further to improve the prediction of loan amount.

For hypothesis 2, ensemble methods, naive bayes and multinomial logistic regression is used for modeling the data. A preliminary Naïve Bayes model was built by sampling 70% of each level in the sector variable. This model didn’t work well. The model was built again using random sampling. Sampling 70% of each level in the sector variable gave low prediction because the size of each stratum obtained after sampling was not proportionate to the size in the original data. Random sampling gave a better performance accuracy and was found to be the best tailored statistical model. Proportional stratified sampling can be implemented to further improve performance.
