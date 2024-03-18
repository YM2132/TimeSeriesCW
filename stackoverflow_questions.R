library(dplyr)
library(zoo)
library(lubridate) # For year() and month()
library(ggplot2)
library(prophet)
library(dygraphs)

data = read.csv("TotalQuestions.csv")

# Sum up the counts of each question for each language per month
data <- data %>%
  mutate(Questions = rowSums(select(., -Month)))

# Convert the Month column to a Date column
data$yearmon <- as.yearmon(data$Month, "%Y-%m")

# Ready for Prophet model
data <- rename(data, ds = Month, y = Questions)

dropped_data <- data.frame(
  ds = as.Date(data$yearmon),
  y = data$y
)

print(dropped_data)

ggplot(data = dropped_data, aes(x = ds, y = y)) + 
  geom_line() + # Draws the line connecting points
  geom_point() + # Adds points at each data point
  labs(x = "Date", y = "Occurrences", title = "Occurrences Over Time") +
  theme_minimal()

# Fit the model
model <- prophet(dropped_data)

# Make a dataframe to hold predictions for the next 12 months
future <- make_future_dataframe(model, periods = 60, freq = "month")

# Generate predictions
preds <- predict(model, future)

# Plot the forecast with yearly dates on the x-axis
plot(model, preds)

# Plot the forecast components
components_plot <- prophet_plot_components(model, preds)

dyplot.prophet(model, preds)

# Print the predicted values
print(preds[['yhat']])

# Ensure both are Date objects
preds$ds <- as.Date(preds$ds)

# Assuming `df` is your original dataset with actual values
historical_forecast <- preds[preds$ds %in% dropped_data$ds, ]
dropped_data$predicted <- historical_forecast$yhat[dropped_data$ds %in% historical_forecast$ds]
dropped_data$residuals <- dropped_data$y - dropped_data$predicted

# Plot the residuals
ggplot(dropped_data, aes(x = ds, y = residuals)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals Over Time", x = "Time", y = "Residuals")

library(lmtest)

# Assuming your residuals and predicted values are correctly calculated
bp_test <- bptest(dropped_data$residuals ~ dropped_data$predicted, data = dropped_data)
print(bp_test)


yearly_data <- dropped_data %>%
  group_by(year = year(ds)) %>%
  summarise(y = sum(y))

yearly_data$y <- log(yearly_data$y)

yearly_data$ds <- as.Date(paste0(yearly_data$year, "-01-01"))
ggplot(data = yearly_data, aes(x = ds, y = y)) + 
  geom_line() + # Draws the line connecting points
  geom_point() + # Adds points at each data point
  labs(x = "Date", y = "Questions", title = "Questions Over Time") +
  theme_minimal()

model_yearly <- prophet(yearly_data)
future <- make_future_dataframe(model, periods = 1, freq = "year")
preds <- predict(model_yearly, future)

plot(model_yearly, preds)

dyplot.prophet(model_yearly, preds)

prophet_plot_components(model_yearly, preds)

# Clear the environment
rm(list=ls())
# Assuming the necessary libraries are already loaded
