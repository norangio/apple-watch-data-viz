Hot Yoga Data Visualization from Apple Watch Data
================
Nick Orangio

``` r
#
#clean up the data set (remove non-yoga workouts, convert data to usable formats)
#

library("chron")

#import data
yoga_data = read.csv('/Users/nickorangio/Main Directory/Personal Projects/R Data Viz/allWorkouts04-May-2019.csv', header = TRUE, stringsAsFactors = FALSE)

yoga_data <- data.frame(yoga_data)

#subset for yoga data
yoga_subset <- subset(yoga_data, Type == "Yoga")

#remove unneeded columns
yoga_subset <- within(yoga_subset, rm(Distance, Average.Speed, Average.Pace))

#convert start and end to dates
yoga_subset$Start <- as.Date(yoga_subset$Start, format = "%Y-%m-%d")
yoga_subset$End <- as.Date(yoga_subset$End, format = "%Y-%m-%d")

#conert duration to minutes
yoga_subset$Duration <- as.character(yoga_subset$Duration)
yoga_subset$Duration <- times(yoga_subset$Duration)
yoga_subset$Duration <- minutes(yoga_subset$Duration) + 60*hours(yoga_subset$Duration)

#remove outlier (was not actually yoga)
yoga_subset <- yoga_subset[-2,]
```

``` r
library("ggplot2")

#kcal by duration
ggplot(yoga_subset, aes(x=Duration, y=Total.Energy.kcal)) + 
  geom_point(aes(colour=Max.Heart.Rate), size = 2.5) +
  theme(axis.text.x = element_text(size = 11, angle = 40, vjust = 0.7)) +
  ylab("Total kcal burned") + xlab("Duration in minutes") +
  geom_smooth(method = lm)
```

![](Apple_Watch_Data_Viz_files/figure-markdown_github/yogaplot-1.png)

Exploratory analysis examining calories burned by duration shows a strong linear relationship, with calories burned increasing with longer durations.

``` r
#kcal by date
ggplot(yoga_subset, aes(x=Start, y=Total.Energy.kcal)) + 
  geom_point(aes(size=Duration, colour=Max.Heart.Rate)) +
  theme(axis.text.x = element_text(size = 11, angle = 40, vjust = 0.5)) +
  ylab("Total kcal burned") + xlab("Start month")
```

![](Apple_Watch_Data_Viz_files/figure-markdown_github/yogaplot2-1.png)

Calories burned over the last year are fairly consistent.

Next, a regression model will be fit to the data to develop a model that predicts total calories using average heart rate, max heart rate, and duration. The data will be split into training data (75% of data) and test data (25% of data).

``` r
#split data
set.seed(101)
sample <- sample.int(n = nrow(yoga_subset), size = floor(0.75*nrow(yoga_subset)), replace = FALSE)
train <- yoga_subset[sample,]
test <- yoga_subset[-sample,]

#fit full regression model
lm_model_full <- lm(Total.Energy.kcal ~ Average.Heart.Rate + Max.Heart.Rate + Duration
               , data = train)

summary(lm_model_full)
```

    ## 
    ## Call:
    ## lm(formula = Total.Energy.kcal ~ Average.Heart.Rate + Max.Heart.Rate + 
    ##     Duration, data = train)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -67.938 -15.260  -0.396  14.952 131.124 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -402.9572   100.6442  -4.004 0.000169 ***
    ## Average.Heart.Rate    5.8644     1.0953   5.354 1.33e-06 ***
    ## Max.Heart.Rate       -1.5063     0.9241  -1.630 0.108180    
    ## Duration              9.4613     0.6228  15.191  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 28.18 on 62 degrees of freedom
    ## Multiple R-squared:  0.8168, Adjusted R-squared:  0.8079 
    ## F-statistic: 92.13 on 3 and 62 DF,  p-value: < 2.2e-16

``` r
mean(lm_model_full$residuals^2)
```

    ## [1] 745.906

A simple linear regression model is a pretty good fit, with a high coefficient of determination (0.81) indicating the model explains much of the varability in total calories burned. The mean squared error on the training data is 745. The summary of the model indicates that average heart rate and duration are statistically significant (p &lt; 0.05) in the model.
