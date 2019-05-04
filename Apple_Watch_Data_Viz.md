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
