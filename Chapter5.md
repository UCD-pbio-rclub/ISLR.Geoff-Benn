# Chapter5



**3. We now review k-fold cross-validation.**

(a) Explain how k-fold cross-validation is implemented.

K-fold cross validation invovles dividing the data in k equally sized groups (usually 5 or 10). The model is then fit k times, with one group being left out each time as the test group. The model fit on k-1 groups is then tested against the group left out. After being run k times, an average test error can be computed.

(b) What are the advantages and disadvantages of k-fold cross validation relative to:
i. The validation set approach?

In the validation set approach, you have two issues:
-you're only testing once and could therefore be misled by an unlucky draw of test/training data.
-often the validation set is relatively large relative to the available data, potentially leading you to over-estimate test error.

ii. LOOCV?
-depending on the statistic LOOCV can be computationally expensive
-the training set is essentially identical each time, so variance could be an issue

**4. Suppose that we use some statistical learning method to make a prediction for the response Y for a particular value of the predictor X. Carefully describe how we might estimate the standard deviation of our prediction.**

I would use a bootstrap approach - I would sample with replacement from the dataset, fit the model, and then make the prediction and repeat 1000 times. I would then calculate the standard deviation. This would be correct, because we're trying to estimate the standard deviation of the prediction, not the accuracy of the model.

**5. In Chapter 4, we used logistic regression to predict the probability of default using income and balance on the Default data set. We will now estimate the test error of this logistic regression model using the validation set approach. Do not forget to set a random seed before beginning your analysis.**

(a) Fit a logistic regression model that uses income and balance to predict default.


```r
library(ISLR)
default <- Default
default.glm1 <- glm(default ~ income + balance, data = default, family = binomial)
summary(default.glm1)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = default)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4725  -0.1444  -0.0574  -0.0211   3.7245  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
## income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
## balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2920.6  on 9999  degrees of freedom
## Residual deviance: 1579.0  on 9997  degrees of freedom
## AIC: 1585
## 
## Number of Fisher Scoring iterations: 8
```


(b) Using the validation set approach, estimate the test error of this model. In order to do this, you must perform the following steps:
i. Split the sample set into a training set and a validation set.


```r
set.seed(1)
library(tidyverse)
```

```
## Loading tidyverse: ggplot2
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library(caTools)
default$set <- sample.split(default$default, SplitRatio = .75)
default.test1 <- subset(default, set == "FALSE")
default.train1 <- subset(default, set == "TRUE")
```


ii. Fit a multiple logistic regression model using only the training observations.


```r
default.glm2 <- glm(default ~ income + balance, data = default.train1, family = binomial)
summary(default.glm2)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = default.train1)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4325  -0.1490  -0.0613  -0.0233   3.6831  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.130e+01  4.834e-01  -23.38  < 2e-16 ***
## income       2.168e-05  5.647e-06    3.84 0.000123 ***
## balance      5.486e-03  2.534e-04   21.65  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2192.2  on 7499  degrees of freedom
## Residual deviance: 1211.3  on 7497  degrees of freedom
## AIC: 1217.3
## 
## Number of Fisher Scoring iterations: 8
```


iii. Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of
default for that individual, and classifying the individual to the default category if the posterior probability is greater
than 0.5.


```r
glm.probs=predict(default.glm2, default.test1, type="response")
glm.pred=rep("No",2500)
glm.pred[glm.probs>.5]="Yes"
```

iv. Compute the validation set error, which is the fraction of the observations in the validation set that are misclassified.


```r
table(glm.pred,default.test1$default)
```

```
##         
## glm.pred   No  Yes
##      No  2413   55
##      Yes    4   28
```

```r
(2409 + 20)/2500
```

```
## [1] 0.9716
```

The overall correct prediction rate was 97.1%, however the model only correctly predicted defaults 25% of the time.

(c) Repeat the process in (b) three times, using three different splits of the observations into a training set and a validation set. Comment on the results obtained.

Same split, with different set.seeds

```r
set.seed(10)
default2 <- default
default2$set <- sample.split(default2$default, SplitRatio = .75)
default.test2 <- subset(default2, set == "FALSE")
default.train2 <- subset(default2, set == "TRUE")
default.glm3 <- glm(default ~ income + balance, data = default.train2, family = binomial)
summary(default.glm3)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = default.train2)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4665  -0.1410  -0.0556  -0.0201   3.7483  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.150e+01  5.014e-01 -22.930   <2e-16 ***
## income       1.601e-05  5.791e-06   2.765   0.0057 ** 
## balance      5.723e-03  2.657e-04  21.542   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2192.2  on 7499  degrees of freedom
## Residual deviance: 1168.0  on 7497  degrees of freedom
## AIC: 1174
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.probs=predict(default.glm3, default.test2, type="response")
glm.pred2=rep("No",2500)
glm.pred2[glm.probs>.5]="Yes"
table(glm.pred2,default.test2$default)
```

```
##          
## glm.pred2   No  Yes
##       No  2404   59
##       Yes   13   24
```

```r
(2404 + 24)/2500
```

```
## [1] 0.9712
```

```r
24/(24+59)
```

```
## [1] 0.2891566
```
97.12% overall and 28.9% correct on the defaults.


```r
set.seed(100)
default3 <- default
default3$set <- sample.split(default3$default, SplitRatio = .75)
default.test3 <- subset(default3, set == "FALSE")
default.train3 <- subset(default3, set == "TRUE")
default.glm4 <- glm(default ~ income + balance, data = default.train3, family = binomial)
summary(default.glm4)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = default.train3)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4323  -0.1438  -0.0593  -0.0221   3.7055  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.136e+01  4.944e-01 -22.982  < 2e-16 ***
## income       1.936e-05  5.768e-06   3.357 0.000788 ***
## balance      5.560e-03  2.571e-04  21.627  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2192.2  on 7499  degrees of freedom
## Residual deviance: 1189.6  on 7497  degrees of freedom
## AIC: 1195.6
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.probs=predict(default.glm4, default.test3, type="response")
glm.pred3=rep("No",2500)
glm.pred3[glm.probs>.5]="Yes"
table(glm.pred3,default.test3$default)
```

```
##          
## glm.pred3   No  Yes
##       No  2409   60
##       Yes    8   23
```

```r
(2409 + 23)/2500
```

```
## [1] 0.9728
```

```r
23/(23+60)
```

```
## [1] 0.2771084
```
97.28% overall and 27.7% correct on the defaults.


```r
set.seed(100)
default4 <- default
default4$set <- sample.split(default4$default, SplitRatio = .5)
default.test4 <- subset(default4, set == "FALSE")
default.train4 <- subset(default4, set == "TRUE")
default.glm5 <- glm(default ~ income + balance, data = default.train4, family = binomial)
summary(default.glm5)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = default.train4)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3328  -0.1543  -0.0663  -0.0260   3.6414  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.081e+01  5.766e-01 -18.755   <2e-16 ***
## income       1.473e-05  7.053e-06   2.089   0.0367 *  
## balance      5.323e-03  3.025e-04  17.597   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1457.0  on 4999  degrees of freedom
## Residual deviance:  820.9  on 4997  degrees of freedom
## AIC: 826.9
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.probs=predict(default.glm5, default.test4, type="response")
glm.pred4=rep("No",2500)
glm.pred4[glm.probs>.5]="Yes"
table(glm.pred4,default.test4$default)
```

```
##          
## glm.pred4   No  Yes
##       No  2410   57
##       Yes   16   56
```

```r
(2410 + 56)/2500
```

```
## [1] 0.9864
```

```r
56/(56+57)
```

```
## [1] 0.4955752
```
98.6% overall and 49.6% correct on the defaults.

Not a ton of variation between different draws with the same split, but large changes when the split changed.

(d) Now consider a logistic regression model that predicts the probability of default using income, balance, and a dummy variable
for student. Estimate the test error for this model using the validation set approach. Comment on whether or not including a
dummy variable for student leads to a reduction in the test error rate.


```r
set.seed(10)
default2 <- default
default2$set <- sample.split(default2$default, SplitRatio = .75)
default.test2 <- subset(default2, set == "FALSE")
default.train2 <- subset(default2, set == "TRUE")
default.glm3 <- glm(default ~ income + balance + student, data = default.train2, family = binomial)
summary(default.glm3)
```

```
## 
## Call:
## glm(formula = default ~ income + balance + student, family = binomial, 
##     data = default.train2)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4633  -0.1393  -0.0540  -0.0195   3.7722  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.089e+01  5.717e-01 -19.043   <2e-16 ***
## income      -3.814e-07  9.748e-06  -0.039   0.9688    
## balance      5.809e-03  2.711e-04  21.432   <2e-16 ***
## studentYes  -5.801e-01  2.768e-01  -2.096   0.0361 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2192.2  on 7499  degrees of freedom
## Residual deviance: 1163.6  on 7496  degrees of freedom
## AIC: 1171.6
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.probs=predict(default.glm3, default.test2, type="response")
glm.pred2=rep("No",2500)
glm.pred2[glm.probs>.5]="Yes"
table(glm.pred2,default.test2$default)
```

```
##          
## glm.pred2   No  Yes
##       No  2405   60
##       Yes   12   23
```

```r
(2405 + 23)/2500
```

```
## [1] 0.9712
```

```r
23/(23+60)
```

```
## [1] 0.2771084
```
97.12% overall and 27.7% correct on the defaults. Not a big change.

**6. We continue to consider the use of a logistic regression model to predict the probability of default using income and balance on the Default data set. In particular, we will now compute estimates for the standard errors of the income and balance logistic regression coefficients in two different ways: (1) using the bootstrap, and (2) using the standard formula for computing the standard errors in the glm() function. Do not forget to set a random seed before beginning your analysis.**

(a) Using the summary() and glm() functions, determine the estimated standard errors for the coefficients associated with income
and balance in a multiple logistic regression model that uses both predictors.


```r
summary(default.glm1)$coef
```

```
##                  Estimate   Std. Error    z value      Pr(>|z|)
## (Intercept) -1.154047e+01 4.347564e-01 -26.544680 2.958355e-155
## income       2.080898e-05 4.985167e-06   4.174178  2.990638e-05
## balance      5.647103e-03 2.273731e-04  24.836280 3.638120e-136
```


(b) Write a function, boot.fn(), that takes as input the Default data set as well as an index of the observations, and that outputs the coefficient estimates for income and balance in the multiple logistic regression model.


```r
boot.fn <- function(data, index) {
  return(coef(glm(default ~ income + balance, data = default, subset = index, family = binomial)))
}
boot.fn(default, 1:10000)
```

```
##   (Intercept)        income       balance 
## -1.154047e+01  2.080898e-05  5.647103e-03
```


(c) Use the boot() function together with your boot.fn() function to estimate the standard errors of the logistic regression coefficients for income and balance.


```r
library(boot)
boot(default, boot.fn, 100)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = default, statistic = boot.fn, R = 100)
## 
## 
## Bootstrap Statistics :
##          original        bias     std. error
## t1* -1.154047e+01 -4.476601e-02 4.729915e-01
## t2*  2.080898e-05 -1.296434e-07 5.184228e-06
## t3*  5.647103e-03  2.850525e-05 2.418275e-04
```


(d) Comment on the estimated standard errors obtained using the glm() function and using your bootstrap function.

The bootstrap estimates of standard error were pretty close to what the GLM gave, but were slightly higher.

**7. In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be used in order to compute the LOOCV test error estimate. Alternatively, one could compute those quantities using just the glm() and predict.glm() functions, and a for loop. You will now take this approach in order to compute the LOOCV error for a simple logistic regression model on the Weekly data set. Recall that in the context of classification problems, the LOOCV error is given in (5.4).**


(a) Fit a logistic regression model that predicts Direction using Lag1 and Lag2.


```r
weekly <- Weekly
dir.glm1 <- glm(Direction ~ Lag1 + Lag2, data = weekly, family = binomial)
summary(dir.glm1)
```

```
## 
## Call:
## glm(formula = Direction ~ Lag1 + Lag2, family = binomial, data = weekly)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.623  -1.261   1.001   1.083   1.506  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.22122    0.06147   3.599 0.000319 ***
## Lag1        -0.03872    0.02622  -1.477 0.139672    
## Lag2         0.06025    0.02655   2.270 0.023232 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1496.2  on 1088  degrees of freedom
## Residual deviance: 1488.2  on 1086  degrees of freedom
## AIC: 1494.2
## 
## Number of Fisher Scoring iterations: 4
```


(b) Fit a logistic regression model that predicts Direction using Lag1 and Lag2 using all but the first observation.


```r
weekly <- rownames_to_column(weekly, var = "ID")
weekly1 <- filter(weekly, ID > 1)
weekly2 <- filter(weekly, ID != 1)
dir.glm2 <- glm(Direction ~ Lag1 + Lag2, data = weekly1, family = binomial)
summary(dir.glm2)
```

```
## 
## Call:
## glm(formula = Direction ~ Lag1 + Lag2, family = binomial, data = weekly1)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.6258  -1.2617   0.9999   1.0819   1.5071  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.22324    0.06150   3.630 0.000283 ***
## Lag1        -0.03843    0.02622  -1.466 0.142683    
## Lag2         0.06085    0.02656   2.291 0.021971 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1494.6  on 1087  degrees of freedom
## Residual deviance: 1486.5  on 1085  degrees of freedom
## AIC: 1492.5
## 
## Number of Fisher Scoring iterations: 4
```


(c) Use the model from (b) to predict the direction of the first observation. You can do this by predicting that the first observation will go up if P(Direction="Up"|Lag1, Lag2) > 0.5. Was this observation correctly classified?


```r
predict(dir.glm2, weekly[1,], type = "response") > 0.5
```

```
##    1 
## TRUE
```
The prediction is up, but the true direction is down.

(d) Write a for loop from i = 1 to i = n, where n is the number of observations in the data set, that performs each of the following
steps:
i. Fit a logistic regression model using all but the ith observation to predict Direction using Lag1 and Lag2.
ii. Compute the posterior probability of the market moving up for the ith observation.
iii. Use the posterior probability for the ith observation in order to predict whether or not the market moves up.
iv. Determine whether or not an error was made in predicting the direction for the ith observation. If an error was made,
then indicate this as a 1, and otherwise indicate it as a 0.


```r
cv.error <- rep(1, dim(weekly)[0])
```

```
## Error in rep(1, dim(weekly)[0]): invalid 'times' argument
```

```r
for (i in 1:dim(weekly)[1]) {
  dir.glm3 <- glm(Direction ~ Lag1 + Lag2, data = weekly[-i,], family = binomial)
  pred <- predict.glm(dir.glm3, weekly[i,], type = "response")
  dir <- ifelse(pred > 0.5, "UP", "DOWN")
  cv.error[i] <- ifelse(weekly[i,]$Direction == dir, 1, 0)
}
```

```
## Error in cv.error[i] <- ifelse(weekly[i, ]$Direction == dir, 1, 0): object 'cv.error' not found
```

```r
mean(cv.error)
```

```
## Error in mean(cv.error): object 'cv.error' not found
```


(e) Take the average of the n numbers obtained in (d)iv in order to obtain the LOOCV estimate for the test error. Comment on the
results.

**9. We will now consider the Boston housing data set, from the MASS library.**

(a) Based on this data set, provide an estimate for the population mean of medv. Call this estimate ˆμ.


```r
library(MASS)
```

```
## 
## Attaching package: 'MASS'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```r
boston <- Boston
mean(boston$medv)
```

```
## [1] 22.53281
```


(b) Provide an estimate of the standard error of ˆμ. Interpret this result.
Hint: We can compute the standard error of the sample mean by dividing the sample standard deviation by the square root of the
number of observations.


```r
sd(boston$medv)/sqrt(nrow(boston))
```

```
## [1] 0.4088611
```


(c) Now estimate the standard error of ˆμ using the bootstrap. How does this compare to your answer from (b)?


```r
sd.err <- function(data, index) {
  return(mean(data[index]))
}
boot(boston$medv, sd.err, 100)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = boston$medv, statistic = sd.err, R = 100)
## 
## 
## Bootstrap Statistics :
##     original     bias    std. error
## t1* 22.53281 0.05420553   0.4196802
```


(d) Based on your bootstrap estimate from (c), provide a 95% confidence interval for the mean of medv. Compare it to the results
obtained using t.test(Boston$medv).
Hint: You can approximate a 95% confidence interval using the formula [ˆμ − 2SE(ˆμ), ˆμ + 2SE(ˆμ)].


```r
#Lower:
  22.53-(2*.41)
```

```
## [1] 21.71
```

```r
#Upper:
  22.53+(2*.41)
```

```
## [1] 23.35
```

```r
t.test(boston$medv)
```

```
## 
## 	One Sample t-test
## 
## data:  boston$medv
## t = 55.111, df = 505, p-value < 2.2e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  21.72953 23.33608
## sample estimates:
## mean of x 
##  22.53281
```
The intervals are pretty close to each other.

(e) Based on this data set, provide an estimate, ˆμmed, for the median value of medv in the population.


```r
median(boston$medv)
```

```
## [1] 21.2
```


(f) We now would like to estimate the standard error of ˆμmed. Unfortunately, there is no simple formula for computing the standard error of the median. Instead, estimate the standard error of the median using the bootstrap. Comment on your findings.


```r
med.err <- function(data, index) {
  return(median(data[index]))
}
boot(boston$medv, med.err, 100)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = boston$medv, statistic = med.err, R = 100)
## 
## 
## Bootstrap Statistics :
##     original  bias    std. error
## t1*     21.2   0.015   0.3897357
```


(g) Based on this data set, provide an estimate for the tenth percentile of medv in Boston suburbs. Call this quantity ˆμ0.1. (You can use the quantile() function.)


```r
quantile(boston$medv, probs = 0.1)
```

```
##   10% 
## 12.75
```


(h) Use the bootstrap to estimate the standard error of ˆμ0.1. Comment on your findings.


```r
quant.err <- function(data, index) {
  return(quantile(data[index],probs = 0.1))
}
boot(boston$medv, quant.err, 100)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = boston$medv, statistic = quant.err, R = 100)
## 
## 
## Bootstrap Statistics :
##     original  bias    std. error
## t1*    12.75  0.0835   0.4362984
```
