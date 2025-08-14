install.packages("readxl")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("ggplot2")
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
setwd("C:/Users/atoju/Toju Capstone Case Study/Capstone excel sheets")
df1 <- read_excel("Divvy_Trips_2019_Q1.xlsx")
df2 <- read_excel("Divvy_Trips_2020_Q1.xlsx")
str(df1)
str(df2)
colnames(df1)
colnames(df2)
head(df1)
head(df2)
df1 <- df1 %>%
  rename(ride_id = trip_id,
         start_station_name = from_station_name,
         start_station_id   = from_station_id,
         end_station_name   = to_station_name,
         end_station_id     = to_station_id)
df2 <- df2 %>%
  rename(start_time = started_at,
         end_time   = ended_at,
         usertype   = member_casual)
colnames(df1)
colnames(df2)
df1 <- df1 %>% 
  select(-bikeid, -gender, -birthyear)
df2 <- df2 %>%
  select(-rideable_type, -start_lat, -start_lng, -end_lat, -end_lng)
colnames(df1)
colnames(df2)
df2$start_time <- as.POSIXct(df2$start_time, tz= "UTC")
df2$end_time <- as.POSIXct(df2$end_time, tz = "UTC")
df2 <- df2 %>%
  mutate (tripduration = as.numeric(difftime(end_time,start_time, units = "secs")))
colnames(df2)
colnames(df1)
summary(df1)
summary(df2)
min_duration <- min(df2$tripduration, na.rm = TRUE)
print (min_duration)
negative_rides <- df2 %>%
  filter(tripduration < 0) %>%
  select (ride_id,start_time,end_time,tripduration)
print(negative_rides)
df2 <- df2 %>%
  mutate(tripduration = if_else(tripduration <= 0 | is.na(tripduration),
                                median(tripduration, na.rm = TRUE),
                                tripduration))
min_duration <- min(df2$tripduration, na.rm = TRUE)
print (min_duration)
summary(df2)
summary(df1)
df1 <- df1 %>%
  mutate(usertype = case_when(
    usertype %in% c("Subscriber") ~ "member",
    usertype %in% c("Customer") ~ "casual",
    TRUE ~ usertype))
view(df1)
colnames(df1)
colnames(df2)
merged_df <- rbind(df1,df2)
str(merged_df)
summary(merged_df)
merged_df %>% select(ride_id,tripduration,usertype) %>% head()
summary(merged_df$tripduration)
merged_df %>%
  summarise(
    mean_duration = mean(tripduration, na.rm = TRUE),
    median_duration = median(tripduration, na.rm = TRUE),
    SD_duration = sd(tripduration, na.rm = TRUE)
  )
member_rides <- merged_df %>%
  filter(usertype == "member") %>%
  select(ride_id,usertype,tripduration)
casual_rides <- merged_df %>%
  filter(usertype == "casual") %>%
  select(ride_id,usertype,tripduration)
head(member_rides)
head(casual_rides)
duration_summary <- merged_df %>%
  group_by(usertype) %>%
  summarise(
    mean_duration = mean(tripduration, na.rm = TRUE),
    median_duration = median(tripduration, na.rm = TRUE),
    min_duration = min(tripduration, na.rm =TRUE),
    max_duration = max(tripduration, na.rm = TRUE),
    total_rides = n()
  )
print(duration_summary)
merged_df <- merged_df %>%
  mutate(day_of_week = wday(start_time, label = TRUE, abbr =  FALSE))
unique(merged_df$day_of_week)
day_freq <- table(merged_df$day_of_week)
mode_day <- names(day_freq)[which.max(day_freq)]
print(mode_day)
print(day_freq)
get_mode <- function(x) {
  tb1 <- table(x)
  names(tb1)[which.max(tb1)]
}
mode_by_user <- merged_df %>%
  group_by(usertype) %>%
  summarise(mode_day = get_mode(day_of_week),
            mode_count = max(table(day_of_week)))
print(mode_by_user)
merged_df <- merged_df%>%
  mutate(tripduration = tripduration/3600)
ggplot(merged_df, aes(x = usertype, y= tripduration, fill = usertype)) +
  geom_boxplot() +
  labs(title = "Ride Duration BY Usertype",
       x = "User Type",
       y = "Ride Duration (hours)") +
  theme_minimal()
ggplot(merged_df, aes(x = tripduration, fill = usertype)) +
geom_histogram(binwidth =1000, alpha = 0.5, position = "identity") +
  labs(title = "Distribution of Ride Duration by Usertype",
       x = "Ride Duration (hours)",
       y = "Frequency") + 
  theme_minimal()
min_by_user <-merged_df %>%
  group_by(usertype) %>% 
  summarise(min_duration = min(tripduration, na.rm = TRUE)) %>%
  arrange(min_duration)
print(min_by_user)
min_rows <-  merged_df %>%
  group_by(usertype) %>%
  filter(tripduration == min(tripduration, na.rm = TRUE)) %>%
  select(ride_id,usertype,tripduration)
print(min_rows)
ggplot(merged_df,aes(x=day_of_week)) +
  geom_bar(fill = "blue", color = "black") +
  labs(title = "Number of Rides by Day of the Week",
       x = "Day of the week",
       Y = "Number of Rides") +
  theme_minimal()
ggplot(merged_df, aes(x = day_of_week, fill =usertype)) +
  geom_bar(position = "dodge") +
  labs(title = "Number of Rides by Day of the Week and Usertype",
       X = "Day of the Week",
       y = "Number of Rides") +
  theme_minimal()
duration_by_day_user <- merged_df %>%
  group_by(usertype, day_of_week) %>%
  summarise(mean_duration = mean(tripduration, na.rm = TRUE),
            median_duration = median(tripduration, na.rm = TRUE),
            min_duration = min(tripduration, na.rm = TRUE),
            Total_rides = n(),
            .groups = "drop"
  )
print(duration_by_day_user)
write.csv(member_rides,"member_rides.csv", row.names = FALSE)
write.csv(casual_rides,"casual_rides.csv", row.names = FALSE)
write.csv(duration_summary,"duration_by_usertype.csv", row.names = FALSE)
write.csv(mode_by_user,"mode_day_by_user.csv", row.names = FALSE)
write.csv(duration_by_day_user,"duration_by_day_User.csv",row.names = FALSE)
write.csv(as.data.frame(table(merged_df$day_of_week)),"day_of_the_week_freq.csv", row.names = FALSE)