#__author__ = "Nate Cutler"
#__email__ = "ncutler211@gmail.com"

# Load necessary libraries
library(tidyverse)

# Visualize distribution of mean_trip_time and mean_distance
hist(bikes$mean_trip_time)
hist(bikes$mean_distance)

# Create a column for total rides for each day
total_rides <- bikes %>%
  group_by(day) %>%
  summarize(total_rides = sum(number_rides))

# Add total rides to bikes dataset by day
bikes <- bikes %>%
  left_join(total_rides, by = "day")

# Find the row with the most rides and capture weather data
most_rides_row <- bikes %>%
  arrange(desc(total_rides)) %>%
  slice(1)

# Plot showing relationship between precipitation and total rides
ggplot(bikes, aes(x = precipitation, y = total_rides)) +
  geom_point()

# Plot showing relationship between snow and total rides
ggplot(bikes, aes(x = snow, y = total_rides)) +
  geom_point()

# Linear regression model: number of rides ~ average temperature
avg_temp_mod <- lm(number_rides ~ avg_temp, data = bikes)
avg_temp_mod_summary <- summary(avg_temp_mod)

# Calculate confidence interval for beta_1
b1 <- avg_temp_mod_summary$coefficients[2, 1] # beta_1
SE_b1 <- avg_temp_mod_summary$coefficients[2, 2] # Standard Error beta_1
b1_ci_upper <- round(b1 + 2 * SE_b1, 2)
b1_ci_lower <- round(b1 - 2 * SE_b1, 2)
confidence_interval <- paste('Our confidence interval is from', b1_ci_lower, 'to', b1_ci_upper)

# Scatter plot of average temperature and number of rides
ggplot(bikes, aes(x = avg_temp, y = number_rides)) +
  geom_point()

# Linear regression line on the scatter plot of avg_temp and number of rides
x_vals <- seq(min(bikes$avg_temp), max(bikes$avg_temp), length.out = nrow(bikes))
b0 <- avg_temp_mod_summary$coefficients[1, 1] # beta_0
y_vect <- b0 + b1 * x_vals
ggplot(bikes, aes(x = avg_temp, y = number_rides)) +
  geom_point() +
  geom_line(aes(x = x_vals, y = y_vect))

# Linear regression model: number of rides ~ average wind speed
wind_mod <- lm(number_rides ~ avg_wind, data = bikes)
wind_mod_summary <- summary(wind_mod)

# Multiple linear regression model including multiple weather variables
multi_mod <- lm(number_rides ~ avg_wind + precipitation + snow + avg_temp, data = bikes)
summary(multi_mod)

# Linear regression models for individual weather variables
prec_mod <- lm(number_rides ~ precipitation, data = bikes)
summary(prec_mod)
snow_mod <- lm(number_rides ~ snow, data = bikes)
summary(snow_mod)

# Feature engineering: Create a new variable 'var_temp' and add it to bikes dataset
bikes <- bikes %>%
  mutate(var_temp = max_temp - min_temp)

# Multiple linear regression model including var_temp
multi_mod_w_var_temp <- lm(number_rides ~ avg_wind + precipitation + snow + avg_temp + var_temp, data = bikes)
summary(multi_mod_w_var_temp)

# Linear regression model: mean rider age ~ average temperature
avg_age_mod <- lm(mean_rider_age ~ avg_temp, data = bikes)
summary(avg_age_mod)

# Interaction effect: Linear regression model for number of rides considering interaction between avg_temp and mean_rider_age
temp_age_mod <- lm(number_rides ~ avg_temp * mean_rider_age, data = bikes)
summary(temp_age_mod)

# Create sequences for plotting the interaction effect
bikes_row_num <- nrow(bikes)
avg_temp_seq <- seq(min(bikes$avg_temp), max(bikes$avg_temp), length.out = bikes_row_num)

# Calculate y-vectors for the interaction effect plot
int_model <- summary(temp_age_mod)
b1 <- int_model$coefficients[2, 1]
temp_y_vec <- int_model$coefficients[1, 1] + int_model$coefficients[2, 1] * avg_temp_seq +
  int_model$coefficients[3, 1] * 1 + int_model$coefficients[4, 1] * avg_temp_seq * 1
age_y_vec <- int_model$coefficients[1, 1] + int_model$coefficients[2, 1] * avg_temp_seq +
  int_model$coefficients[3, 1] * 0 + int_model$coefficients[4, 1] * avg_temp_seq * 0

# Plot the interaction effect
ggplot(bikes, aes(x = avg_temp, y = number_rides)) +
  geom_point() +
  geom_line(aes(x = avg_temp_seq, y = temp_y_vec, color = 'blue')) +
  geom_line(aes(x = avg_temp_seq, y = age_y_vec, color = 'red'))
