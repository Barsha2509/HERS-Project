# HERS-Project
This dataset is from HERS clinical trial of hormone therapy in postmenopausal women.  The aim of this project is to explore possible factors that can predict the level of  LDL(Low density Lipoprotein) cholesterol levels in the postmenopausal women. 
The analyzed factors consists of 7 continuous variables, including DBP (Diastolic Blood Pressure),SBP(Systolic Blood Pressure), age, TG(Triglycerides),  BMI(Body Mass Index), HDL(High density Lipoprotein cholesterol), glucose(glucose level), and 2 categorical variables, including smoking and race.
Linear regression analysis method was used for this project. First, data cleaning was done and then visually analyzed the distribution and correlation of each variables in raw data. After that, the raw dataset was divided into training set and testing set by the ratio of 2:1. 
Next, the analysis and diagnostics of training data was performed. First, training dataset was visually analyzed with the help ofbox plot, histogram , qqplot, scatter matrix and correlation matrix to get the view of the data distribution and correlation. 
Then, preliminary model was fitted on the data using first order full model and after that model diagnostics were performed based on the assumptions of linearity, homoscedasticity and normality, as well as the independence.
To remedy preliminary model, Y and some X variables were transformed using Box-Cox, which yielded the second model. Then the interaction of some predictors was  tested based on the diagnosis result of second model. 
Next, model selection was done using all the first order predictors and interaction terms by Mallow’s Cp criterion. Lastly,the influential points and outliers were assessed with the selected top 2 best models and validated them in testing data set.


