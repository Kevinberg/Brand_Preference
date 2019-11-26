---
title: "brand_preference"
author: "Kevin Bergmeijer"
date: "21/11/2019"
output: html_document
---
This report aims to make predictions by using R.
To make the predictions, we will be using two data sets: a complete data set, and an incomplete data set.
The idea is to complete the data set that is incomplete by taking the assumption that the essential variables will provide us  a more accurate performance on brand preferences which are
We will get brand preferences by using age and salary as more correlated variables.
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)jjjj
```

```{r}
# Install packages:

pacman::p_load(pacman, caret, dplyr, rmarkdown, ggplot2, C50)

#Load data:

getwd()

com_survey <- read.csv("CompleteResponses.csv")
incom_survey <- read.csv("SurveyIncomplete.csv")

```

```{r}
# Initial exploring

summary(com_survey)
summary(incom_survey)
head(incom_survey)
head(com_survey)
dim(incom_survey)
dim(com_survey)
table(com_survey$brand)

str(com_survey)
str(incom_survey)

# Checking NA's - No NA's 

sum(is.na(incom_survey))

# Merge dataframes - Looking at the distribution of the data

# merge two datasets into one dataframe called dataset_merged. 
#Added another column that shows if coming from the complete or the incomplete survey (complete = Y, incomplete = N)

com_survey$complete <- "Y"
incom_survey$complete <- "N"

dataset_merged_surveys <- rbind(com_survey, incom_survey)
dataset_merged_surveys$complete <- as.factor(dataset_merged_surveys$complete)


# Checking for Outliers

OutlierDataset <- dataset_merged_surveys
OutlierColumn <- dataset_merged_surveys$age
dataset_merged_surveys <- OutlierDataset[OutlierColumn > (quantile(OutlierColumn)[[2]] -
                                                       1.5*IQR(OutlierColumn)),]
boxplot(OutlierColumn)
CompleteResponses <- OutlierDataset[OutlierColumn < (quantile(OutlierColumn)[[4]] +
                                                       1.5*IQR(OutlierColumn)),]
boxplot(dataset_merged_surveys)

# CORRELATION ####

NumericasCompleteRespones <- dataset_merged_surveys[,c(1,2,6,7)]
cor((dataset_merged_surveys [,c(1,2,6)]))
print(cor((dataset_merged_surveys [,c(1,2,6)])))
chisq.test(com_survey$elevel, com_survey$brand)
chisq.test(com_survey$zipcode, com_survey$brand)
chisq.test(com_survey$car,com_survey$brand)
datos.unique <- unique(com_survey) #remover datos
dim(datos.unique)


```

```{r}
```

```{r}
# Changing to right classes 

com_survey$elevel <- as.factor(com_survey$elevel)
com_survey$brand <- as.factor(com_survey$brand)
incom_survey$brand <- as.factor(incom_survey$brand)
incom_survey$elevel <- as.factor(incom_survey$elevel)
com_survey$car <- as.factor(com_survey$car)
com_survey$zipcode <- as.factor(com_survey$zipcode)
incom_survey$car <- as.factor(incom_survey$car)
incom_survey$zipcode <- as.factor(incom_survey$zipcode)
```

For the Complete Data set, we have 7488 observations with seven variables, which are: car type, zip code, salary, level, credit, age, and brand preference.

For the Incomplete Data set, we have 5000 observations with the same variables of the Complete Data set except for brand preference.
 There are not N.A. values, not duplicated columns.
We have converted some variables to a factor and also checked for outliers. 

We will be using the Complete Data set to make our predictions; as we can see, the data set is unbalanced, so we decided to get a merged one to have a more balanced one. 

About correlations, there is not too much that we can conclude for now.
Even we have been using 2 types of analysis. One for the numerical variables (Salary, age and credit).

For categorical variables, we have been using chisq.test and anova. 
No further conclusions can be taken.
For both methods, correlation values are very small, which tells us that further analysis should be taken into consideration.





```{r}




#check distributions for each variable to see of two samples come from the same population
#salary
ggplot(dataset_merged_surveys, aes( y = salary )) + geom_boxplot() + facet_grid(.~complete)
ggplot(dataset_merged_surveys, aes( x = salary )) + geom_density() + facet_grid(.~complete)

#age
ggplot(dataset_merged_surveys, aes( y = age )) + geom_boxplot() + facet_grid(.~complete)
ggplot(dataset_merged_surveys, aes( x = age )) + geom_density() + facet_grid(.~complete)

#elevel
ggplot(dataset_merged_surveys, aes( x = elevel )) + geom_bar() + facet_grid(.~complete)

#zipcode
ggplot(dataset_merged_surveys, aes( x = zipcode )) + geom_bar() + facet_grid(.~complete)


#credit
ggplot(dataset_merged_surveys, aes( y = credit )) + geom_boxplot() + facet_grid(.~complete)
ggplot(dataset_merged_surveys, aes( x = credit )) + geom_density() + facet_grid(.~complete)

#overall we can see that the features have similar distributions.
#Therefore we assume that the sample with the missing data are coming
#from the same poplation
```


```{r}

# 

CompleteResponsesTrue <-subset(com_survey,com_survey$brand==1)
CompleteResponsesFalse <-subset(com_survey,com_survey$brand==0)
additionalTrueResponse <- nrow(CompleteResponsesTrue) - nrow(CompleteResponsesFalse )
CompleteResponsesTrue <-CompleteResponsesTrue[-sample(1:nrow(CompleteResponsesTrue), additionalTrueResponse),]
CompleteResponses <- rbind(CompleteResponsesTrue,CompleteResponsesFalse)
```

For the Complete Data set, we have 7488 observations with seven variables, which are: car type, zip code, salary, level, credit, age, and brand preference.

For the Incomplete Data set, we have 5000 observations with the same variables of the Complete Data set except for brand preference.
 There are not N.A. values, not duplicated columns.
We have converted some variables to a factor and also checked for outliers. 

We will be using the Complete Data set to make our predictions; as we can see, the data set is unbalanced, so we decided to get a merged one to have a more balanced one.


```{r}
# Exploring the data

ggplot(dataset_merged_surveys, aes(x = salary, y = credit)) + geom_smooth(fill = "lightblue", col = "darkgreen") + ggtitle("Salary VS Credit") + theme(plot.title = element_text(hjust = 0.5, colour = "darkblue"))

ggplot(CompleteResponses,aes(x = salary, y = age, colour = brand)) + geom_point() + geom_smooth() + ggtitle("Salary VS Age + Brand Preference") + theme(plot.title = element_text(hjust = 0.5, colour = "darkblue")) + scale_color_hue(name="Brand Preference",breaks=c("0", "1"), labels=c("Acer", "Sony"))


ggplot(CompleteResponses, aes(x = brand, y = salary, fill = brand)) + geom_boxplot() + 
  stat_summary(fun.y = median, colour = "darkblue", geom = "text", vjust = -0.7, aes(label = round(..y.., digits = 1))) + ggtitle("Boxplot Salary with Brand Preference + Median") + theme(plot.title = element_text(hjust = 0.5, colour = "darkblue")) 

```
```{r eval=FALSE, include=FALSE}
```


For instance, we can say that the distributions are very similar, wich is why we can assume that both data sets come from the same population.
Regarding the Salary vs. Credit graphic, it can be said that the less salary is received, the most credit can get.
Also, in other graphics, we can see that data is spread so that no further conclusions can be taken into consideration.




```{r eval=FALSE, include=FALSE}

# Running Model //C50 & RF ###

set.seed(998)
CompleteResponses <- CompleteResponses[1:7]
run_model <- CompleteResponses[sample(1:nrow(CompleteResponses), 7488,replace=FALSE),]
inTraining <- createDataPartition(run_model$brand, p = .75, list = FALSE)
training <- run_model[inTraining,]
testing <- run_model[-inTraining,]

# Fitcontrol settings

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)

#MODEl 1 with basic hyper parameters - Train
levels(training$brand) <- c("ACER" , "SONY")
rfFit1 <- train(x = training[-7], y= training$brand, data = training, method = "rf", trControl= fitControl, tuneLength = 1)
plot(varImp(rfFit1))
varImp(rfFit1)

#- Predict on test data

BP1 <- predict(rfFit1, newdata = testing)
levels(testing$brand) <- c('ACER','SONY')
outcome1 <- postResample(testing$brand,BP1)
outcome1

# Model 2- C5.0

rfFit2 <- train(x = training[-7], y= training$brand, data = training, method = "C5.0Tree", trControl=fitControl, tuneLength = 2)
levels(training$brand) <- c("ACER" , "SONY")
rfFit2



# Model 3 - RF with optimised parameters 

rfFit3 <- train(brand ~ age + salary , data = training , method = "rf", trControl=fitControl, tunegrid = expand.grid(mtry=c(1,2,3,4,5)))
rfFit3
plot(varImp(rfFit3))
varImp(rfFit3)

#- Predict on test data 

BP3 <- predict(rfFit3, newdata = testing)
levels(testing$brand) <- c('ACER','SONY')
outcome3 <- postResample(testing$brand,BP3)
outcome3

best_model <- list(rfFit1, rfFit2, rfFit3)

final_outcome <- predict(rfFit3, newdata = incom_survey)
final_outcome
table(final_outcome)



```

It's important to mention that for feature selection, we were very careful, which is why we add the varimp function to study the more important variables, which are: salary, brand, and age.
Also, the car looks like a useful variable. Still, when we fit the model with it, it does not seen like make the model better, in terms of selecting the model with the best accuracy we decided to use Rf with optimized parameters that are shown in the table above.
We can conclude that the most favorite brand is Sony, with 60 %. 
