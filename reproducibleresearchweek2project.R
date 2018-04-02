#Reading files
activity_data <- read.csv("activity.csv")

#Calculating total steps
steps <- aggregate(activity_data$steps, by = list(Date = activity_data$date), FUN = sum)
names(steps)[names(steps) == "x"] <- "Total"
temp <- as.Date(steps$Date, "%Y-%m-%d")
steps$Date <- format(temp, format = "%m-%d")
head(steps)

#Histogram of total steps
library(ggplot2)
hist_totalSteps1 <- ggplot(data = na.omit(steps), aes(Total)) +
    geom_histogram() +
    xlab("Total Number of Steps Each Day") +
    ylab("Count") +
    ggtitle("Total Number of Steps Taken Each Day Histogram")
print(hist_totalSteps1)

#Calculating mean and median of total steps each day
mean(na.omit(steps$Total))
median(na.omit(steps$Total))
    
#Time series line plot to determine average number of steps taken each day
five_minute_steps <- aggregate(steps ~ interval, data = activity_data, FUN = mean)
TimeSeriesPlot <- ggplot(data = five_minute_steps, aes(x = interval, y = steps)) +
    geom_line() +
    xlab("Time Intervals (5 min)") +
    ylab("Total Number of Steps") +
    ggtitle("Average Number of Steps Taken at 5-Minute Intervals")
print(TimeSeriesPlot)

#Finding which 5-min interval has maximum steps
five_minute_steps[which(five_minute_steps$steps == max(five_minute_steps$steps)),]

#Inputing missing values
library(dplyr) 
input_mean_for_na <- function(num) replace(num, is.na(num), mean(num, na.rm = TRUE))
replacedDays <- (activity_data %>% group_by(interval) %>% mutate(steps = input_mean_for_na(steps)))

#New dataset with replaced days
new_data <- as.data.frame(replacedDays)

#Histogram with replaced days
replaced_steps <- aggregate(new_data$steps, by = list(new_data$date), FUN = sum)
names(replaced_steps)[names(replaced_steps) == "x"] <- "Total"
names(replaced_steps)[names(replaced_steps) == "Group.1"] <- "Date"
hist_totalSteps2 <- ggplot(data = replaced_steps, aes(Total)) +
    geom_histogram() +
    xlab("Total Number of Steps Each Day") +
    ylab("Count") +
    ggtitle("Total Number of Steps Taken Each Day With Replaced NA Values Histogram")
print(hist_totalSteps2)

#Combining hist_totalSteps1 and 2
library(grid)
library(gridExtra)
grid.arrange(hist_totalSteps1, hist_totalSteps2, ncol = 2)

#Comparing means and medians
mean(na.omit(steps$Total))
mean(replaced_steps$Total)
median(na.omit(steps$Total))
median(replaced_steps$Total)

#Panel plot to compare weekdays and weekends
new_data$WeekendOrWeekday <- ifelse(weekdays(as.Date(new_data$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")

new_data <- (new_data %>% group_by(interval, WeekendOrWeekday) %>% summarise(Mean = mean(steps)))
ggplot(new_data, mapping = aes(x = interval, y = Mean)) +
    geom_line() +
    facet_grid(WeekendOrWeekday ~ .) +
    xlab("Interval") +
    ylab("Mean of Steps") +
    ggtitle("Weekday vs. Weekend Average Number of Steps in Each Interval")








