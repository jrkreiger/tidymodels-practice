# Import needed libraries
library(tidymodels)
library(readr)

# Load dataset
insured <-
  read_csv("/Users/jennykreiger/Desktop/insurance.csv") %>%

    # Transform categorical vars into factors
  mutate(sex = factor(sex, levels = c("female", "male")),
         smoker = factor(smoker, levels = c("yes", "no")),
         region = factor(region, levels = c("southwest", "southeast", "northwest", "northeast")))


# View cleaned dataset
insured

# Visualize basic linear models for each region
ggplot(insured, 
       aes(x = bmi,
           y = charges,
           group = region,
           col = region)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + 
  scale_color_viridis_d(option = "plasma", end = .7)

# Initialize a linear regression (OLS) model
lm_mod <-
  linear_reg() %>%
  set_engine("lm")

# Fit the model and view output
lm_fit <-
  lm_mod %>%
  fit(charges ~ (bmi + region)^2, data = insured)
lm_fit

# View tidy output
tidy(lm_fit)

# Predict width for bmi = 27 across all regions
new_points <- expand.grid(bmi = 27,
                          region = c("southwest", "southeast", "northwest", "northeast"))
new_points

# Get predictions
mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

# Calculate confidence intervals
conf_int_pred <- predict(lm_fit,
                         new_data = new_points,
                         type = "conf_int")
conf_int_pred

# Prep predictions and CIs for plotting
plot_data <- 
  new_points %>%
  bind_cols(mean_pred) %>%
  bind_cols(conf_int_pred)

# Visualize predictions and CIs
ggplot(plot_data, aes(x = region)) + 
  geom_point(aes(y = .pred,
                 col = region)) +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper,
                    col = region),
                width = .2) +
  labs(y = "Insurance cost")

# Visualize basic linear models for smokers v. non-smokers
ggplot(insured, 
       aes(x = bmi,
           y = charges,
           group = smoker,
           col = smoker)) + 
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + 
  scale_color_viridis_d(option = "plasma", end = .7)

# Initialize a linear regression (OLS) model
lm_mod <-
  linear_reg() %>%
  set_engine("lm")

# Fit the model and view output
lm_fit <-
  lm_mod %>%
  fit(charges ~ (bmi + smoker)^2, data = insured)
lm_fit

# View tidy output
tidy(lm_fit)

# Predict width for bmi = 27 across all regions
new_points <- expand.grid(bmi = 27,
                          smoker = c("yes", "no"))
new_points

# Get predictions
mean_pred <- predict(lm_fit, new_data = new_points)
mean_pred

# Calculate confidence intervals
conf_int_pred <- predict(lm_fit,
                         new_data = new_points,
                         type = "conf_int")
conf_int_pred

# Prep predictions and CIs for plotting
plot_data <- 
  new_points %>%
  bind_cols(mean_pred) %>%
  bind_cols(conf_int_pred)

# Visualize predictions and CIs
ggplot(plot_data, aes(x = smoker)) + 
  geom_point(aes(y = .pred,
                 col = smoker)) +
  geom_errorbar(aes(ymin = .pred_lower,
                    ymax = .pred_upper,
                    col = smoker),
                width = .2) +
  labs(y = "Insurance cost")

# Initialize a linear regression (OLS) model
lm_mod <-
  linear_reg() %>%
  set_engine("lm")

# Fit the model and view output
lm_fit <-
  lm_mod %>%
  fit(charges ~ (age + sex + bmi + children + smoker + region), data = insured)

# View tidy output
tidy(lm_fit)
