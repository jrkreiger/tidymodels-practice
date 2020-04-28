# This is my own copy of the tutorial code, using the tutorial dataset.
# I haven't adapted the tutorial code to my own dataset and questions yet.

# Import needed packages
library(tidymodels)
library(nycflights13)
library(skimr)

# Set random seed
set.seed(123)

# Mutate data
flight_data <-
  flights %>%
  mutate(
    
    # Change arr_delay to a factor if delay >= 30 min.
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    
    # Extract date from time_hour
    date = as.Date(time_hour)
  ) %>%
  
  # Join weather dataset
  inner_join(weather, by = c("origin", "time_hour")) %>%
  
  # Select desired columns
  select(dep_time, flight, origin, dest, air_time, distance, carrier,
         date, arr_delay, time_hour) %>%
  
  # Drop missing values
  na.omit() %>%
  
  # Change any string values to factors
  mutate_if(is.character, as.factor)

# Inspect counts of arr_delay values
flight_data %>%
  count(arr_delay) %>%
  mutate(prop = n/sum(n))

# Inspect categorical variables
flight_data %>%
  skimr::skim(dest, carrier)

# Set another random seed
set.seed(555)

# Split off training data
data_split <- initial_split(flight_data, prop = 3/4)

# Convert train and test sets to dataframes
train_data <- training(data_split)
test_data <- testing(data_split)

# Compose recipe for model
flights_rec <-
  recipe(arr_delay ~ ., data = train_data) %>%
  
  # Tell model to consider flight and time_hour as IDs only, not predictors
  update_role(flight, time_hour, new_role = "ID") %>%
  
  # Split date column into day of week and month columns
  step_date(date, features = c("dow", "month")) %>%
  
  # Create a new column to record whether a date was a US holiday
  step_holiday(date, holidays = timeDate::listHolidays("US")) %>%
  
  # Drop original date column
  step_rm(date) %>%
  
  # Convert original column to dummy variables
  step_dummy(all_nominal(), -all_outcomes()) %>%
  
  # Remove any columns that contain only zeroes
  step_zv(all_predictors())

# Instantiate model
lr_mod <-
  logistic_reg() %>%
  set_engine("glm")

# Create preprocessing + model workflow
flights_wflow <-
  workflow() %>%
  add_model(lr_mod) %>%
  add_recipe(flights_rec)
flights_wflow

# Fit workflow to training data
flights_fit <-
  flights_wflow %>%
  fit(data = train_data)

# Extract the fitted model and view coefficients
flights_fit %>%
  pull_workflow_fit() %>%
  tidy()

# Predict on the test data
flights_pred <-
  predict(flights_fit, test_data, type = "prob") %>%
  bind_cols(test_data %>% select(arr_delay, time_hour, flight))
flights_pred

# Evaluate model with ROC curve
flights_pred %>%
  roc_curve(truth = arr_delay, .pred_late) %>%
  autoplot()

# Evaluate model wtih AUC
flights_pred %>%
  roc_auc(truth = arr_delay, .pred_late)