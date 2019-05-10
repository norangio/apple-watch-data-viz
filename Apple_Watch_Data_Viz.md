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

Next, a regression model will be fit to the data to develop a model that predicts total calories using average heart rate, max heart rate, and duration.

``` r
#fit full regression model
lm_model_full <- lm(Total.Energy.kcal ~ Average.Heart.Rate + Max.Heart.Rate + Duration
               , data = yoga_subset)

summary(lm_model_full)
```

    ## 
    ## Call:
    ## lm(formula = Total.Energy.kcal ~ Average.Heart.Rate + Max.Heart.Rate + 
    ##     Duration, data = yoga_subset)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -69.909 -12.436   1.858  16.021 137.633 
    ## 
    ## Coefficients:
    ##                     Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)        -374.7580    82.2336  -4.557 1.74e-05 ***
    ## Average.Heart.Rate    4.9927     0.9206   5.423 5.52e-07 ***
    ## Max.Heart.Rate       -1.0585     0.7879  -1.343    0.183    
    ## Duration              9.5394     0.5525  17.267  < 2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 26.77 on 84 degrees of freedom
    ## Multiple R-squared:  0.8081, Adjusted R-squared:  0.8012 
    ## F-statistic: 117.9 on 3 and 84 DF,  p-value: < 2.2e-16

``` r
mean(lm_model_full$residuals^2)
```

    ## [1] 684.2156

A simple linear regression model is a pretty good fit, with a high coefficient of determination (0.81) indicating the model explains much of the varability in total calories burned. The mean squared error on the training data is 745. The summary of the model indicates that average heart rate and duration are statistically significant (p &lt; 0.05) in the model.

``` r
library("DAAG")
```

    ## Loading required package: lattice

``` r
#run cross validation
yoga_total_cv <- cv.lm(yoga_subset, lm_model_full, m = 4)
```

    ## Analysis of Variance Table
    ## 
    ## Response: Total.Energy.kcal
    ##                    Df Sum Sq Mean Sq F value  Pr(>F)    
    ## Average.Heart.Rate  1  28886   28886    40.3 1.1e-08 ***
    ## Max.Heart.Rate      1  10954   10954    15.3 0.00019 ***
    ## Duration            1 213700  213700   298.1 < 2e-16 ***
    ## Residuals          84  60211     717                    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

![](Apple_Watch_Data_Viz_files/figure-markdown_github/crossval-1.png)

    ## 
    ## fold 1 
    ## Observations in test set: 22 
    ##                       6      7     10    11     26     39    40    41
    ## Predicted         703.5 738.32 650.06 625.9 689.39 647.70 625.9 692.5
    ## cvpred            705.6 739.83 649.87 627.3 688.77 648.87 631.1 694.1
    ## Total.Energy.kcal 715.9 742.41 644.46 651.5 691.13 653.29 664.7 708.7
    ## CV residual        10.3   2.57  -5.41  24.3   2.36   4.43  33.7  14.6
    ##                       46    61     64  71    72    73    87     90      91
    ## Predicted         641.78 608.9 667.15 673 661.8 619.2 663.5 621.99 639.000
    ## cvpred            642.05 613.6 667.49 673 664.0 620.8 663.9 622.63 637.920
    ## Total.Energy.kcal 639.59 596.6 665.11 653 639.4 598.5 679.6 620.65 638.714
    ## CV residual        -2.46 -17.1  -2.38 -20 -24.6 -22.2  15.7  -1.98   0.794
    ##                      92 106   107   124   125
    ## Predicted         647.8 717 658.0 628.5 638.3
    ## cvpred            649.5 719 659.6 632.5 643.8
    ## Total.Energy.kcal 635.7 740 671.2 558.6 579.6
    ## CV residual       -13.8  21  11.5 -73.8 -64.3
    ## 
    ## Sum of squares = 14502    Mean square = 659    n = 22 
    ## 
    ## fold 2 
    ## Observations in test set: 22 
    ##                        1     5    17    29     34     35    38    44
    ## Predicted         752.38 603.5 467.0 649.7 752.45 572.66 637.4 620.6
    ## cvpred            760.67 603.4 460.0 654.1 755.55 569.49 639.7 621.4
    ## Total.Energy.kcal 757.11 562.8 494.5 609.2 748.90 566.74 661.5 646.0
    ## CV residual        -3.56 -40.6  34.5 -44.8  -6.66  -2.75  21.7  24.6
    ##                       50     62    66  69     75    82     88    93    96
    ## Predicted         628.08 651.32 758.9 624 639.71 788.4 617.99 663.3 580.6
    ## cvpred            623.84 655.76 766.1 625 639.73 801.3 619.48 668.5 583.9
    ## Total.Energy.kcal 629.40 649.29 745.8 626 634.04 754.9 628.66 651.9 568.3
    ## CV residual         5.57  -6.47 -20.3   1  -5.68 -46.4   9.18 -16.6 -15.6
    ##                      98   100   104    118   122
    ## Predicted         649.3 582.1 614.0 650.31 621.9
    ## cvpred            656.1 581.6 618.5 651.88 620.3
    ## Total.Energy.kcal 614.9 585.2 597.6 654.00 598.0
    ## CV residual       -41.2   3.6 -20.8   2.12 -22.2
    ## 
    ## Sum of squares = 11911    Mean square = 541    n = 22 
    ## 
    ## fold 3 
    ## Observations in test set: 22 
    ##                        9    14    28    33    36    37    42     47    58
    ## Predicted         616.59 622.7 804.1 618.8 609.0 626.1 663.1 706.29 655.2
    ## cvpred            613.15 619.9 790.0 615.9 607.6 623.5 658.7 697.49 649.9
    ## Total.Energy.kcal 618.81 638.8 807.6 637.0 639.3 665.9 682.5 690.46 639.0
    ## CV residual         5.66  18.9  17.7  21.1  31.6  42.4  23.8  -7.03 -10.9
    ##                      60    65     67    68     70     76     77  83     95
    ## Predicted         677.9 622.0 640.39 636.6 652.65 645.56 662.10 611 622.45
    ## cvpred            671.7 618.9 636.46 633.0 648.28 640.48 655.49 610 619.53
    ## Total.Energy.kcal 685.5 630.1 645.93 652.8 644.95 635.17 652.26 628 616.58
    ## CV residual        13.8  11.2   9.47  19.7  -3.33  -5.31  -3.23  18  -2.95
    ##                      97     99   108 126
    ## Predicted         598.6 664.35 568.6 787
    ## cvpred            597.0 658.46 569.3 772
    ## Total.Energy.kcal 615.6 667.74 594.4 924
    ## CV residual        18.7   9.28  25.1 152
    ## 
    ## Sum of squares = 30017    Mean square = 1364    n = 22 
    ## 
    ## fold 4 
    ## Observations in test set: 22 
    ##                      12    16     24    25     32    43    49    59    63
    ## Predicted         582.1 724.7 600.56 685.4 599.40 619.9 707.1 597.1 755.5
    ## cvpred            579.1 729.9 599.50 687.7 599.79 619.4 711.3 595.7 761.3
    ## Total.Energy.kcal 591.4 703.7 602.53 650.1 606.11 637.1 699.4 618.3 738.7
    ## CV residual        12.3 -26.2   3.03 -37.6   6.31  17.7 -11.9  22.6 -22.7
    ##                      85     89     94    105 109 110   111   113   116
    ## Predicted         623.5 629.86 636.25 615.92 637 622 619.1 609.8 611.8
    ## cvpred            623.9 630.88 638.47 615.71 638 622 619.2 610.5 611.6
    ## Total.Energy.kcal 612.8 633.41 629.84 622.87 625 653 634.0 589.7 595.1
    ## CV residual       -11.2   2.53  -8.63   7.17 -13  31  14.7 -20.8 -16.5
    ##                     120 121   127   131
    ## Predicted         621.1 746 705.2 663.4
    ## cvpred            619.9 752 708.9 665.2
    ## Total.Energy.kcal 560.1 699 738.3 685.0
    ## CV residual       -59.8 -53  29.4  19.8
    ## 
    ## Sum of squares = 13735    Mean square = 624    n = 22 
    ## 
    ## Overall (Sum over all 22 folds) 
    ##  ms 
    ## 797

``` r
#manually calculate r squared (SSR/SST or 1-SSE/SST) based on cv results 
sse <- sum((yoga_total_cv$Total.Energy.kcal - yoga_total_cv$cvpred)^2)

sst <- sum((yoga_total_cv$Total.Energy.kcal - mean(yoga_total_cv$Total.Energy.kcal))^2)

r_squared <- 1 - sse / sst
r_squared
```

    ## [1] 0.776

The figure and code above depicts four regression fits generated from the four-fold cross validation performed in the cv.lm() function.

The coefficent of determination (R squared) is manually calculated using the formula 1 - (SSE / SST), using the predictons generated by the cv.lm() function. The coefficient of determination of 0.78 is smaller than the 0.81 provided in the output of the lm() model, however not by much. This suggests a regression model using all predictors (average heart rate, max heart rate, and duration) is a robust model, even when tested on data not used for training.
