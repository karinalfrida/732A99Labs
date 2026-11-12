library(tidyverse)

# Data in pima-indians-diabetes.csv:
# 1. Number of times pregnant.
# 2. Plasma glucose concentration at 2 hours in an oral glucose tolerance test.
# 3. Diastolic blood pressure (mm Hg).
# 4. Triceps skinfold thickness (mm).
# 5. 2-Hour serum insulin (mu U/ml).
# 6. Body mass index (weight in kg/(height in m)^2).
# 7. Diabetes pedigree function.
# 8. Age (years).
# 9. Diabetes (0=no or 1=yes).

# import user-definied functions
source("R/lab1_assignment3_functions.R")

# read data
A3_pimadata <- read.csv("data/pima-indians-diabetes.csv")

# any NA:s?
if (anyNA(A3_pimadata)) {
  message("NA:s in table")
}

# create tibble with meaningful column names
A3_pimatibble_raw <-
  A3_pimadata |>
  tibble() |>
  magrittr::set_colnames(c("Parity", "Glucose", "DBP", "Skinfold",
                           "Insulin","BMI", "Pedigree", "Age", "Diabetes")) |>
  mutate(Diabetes = as.logical(Diabetes)) |>
  mutate(ID = row_number(), .before = Parity) |>
  select(ID, Age, Pedigree, Parity, BMI, Skinfold,
         DBP, Insulin, Glucose, Diabetes)

# data check
  A3_pimatibble_raw |>
  pivot_longer(-c(ID,Diabetes),
               names_to = "Measure",
               values_to = "Value") |>
  ggplot() +
  aes(x=Value, fill = Diabetes) +
  geom_histogram(bins = 30) +
  scale_fill_manual(labels = c("Non-diabetic", "Diabetic"),
                      values = c("royalblue", "maroon2")) +
  facet_wrap(~ Measure, scales = "free", nrow = 2) +
  theme_bw() +
  theme(legend.position = "top")

# checking exact range for Age, Glucose, and Diabetes
A3_pimatibble_raw |>
  select(c(Age, Glucose, Diabetes)) |>
  map(\(x) range(x))

# removing cases with Glucose = 0
A3_pimatibble <-
  A3_pimatibble_raw |>
  filter(Glucose != 0)

# saving adjusted ranges
A3_minmaxlist <-
  A3_pimatibble |>
  select(Age, Glucose) |>
  map(\(x) tibble(min = min(x), max = max(x)))

A3_Age_range <- unlist(A3_minmaxlist[["Age"]])
A3_Glucose_range <- unlist(A3_minmaxlist[["Glucose"]], use.names = FALSE)

# scatterplot Glucose vs Age

A3_Glucose_vs_Age_scatter <-
  A3_pimatibble |>
  ggplot() +
  aes(x = Age, y = Glucose, color = Diabetes) +
  geom_point(size = 0.5, alpha = 0.8) +
  theme_bw() +
  theme(legend.position = "top") +
  scale_colour_manual(labels = c("Non-diabetic", "Diabetic"),
                      values = c("royalblue", "maroon2")) +
  scale_x_continuous(
    limits = A3_Age_range + c(-5, +5),
    breaks = scales::breaks_width(width=10)) + # equally spaced breaks
  scale_y_continuous(
    limits = A3_Glucose_range + c(-5, +5),
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(y = "Plasma glucose")

# training a logistic regression model

# Diabetes ~ Glucose + Age in a logistic regression model
# using glm()= with family = "binomial" since target has two levels

A3_logistic_model <-
  glm(Diabetes ~ Glucose + Age, data = A3_pimatibble, family = "binomial")

summary(A3_logistic_model)

# coefficients (= coefficients in the probabilistic equation)

A3_logistic_coef <- coef(A3_logistic_model)

A3_theta0 <- A3_logistic_coef["(Intercept)"]
A3_theta1 <- A3_logistic_coef["Glucose"]
A3_theta2 <- A3_logistic_coef["Age"]

# adding predicted probabilities to data
A3_pimatibble_pred_probs <-
  A3_pimatibble |>
  mutate(Predicted_prob = predict(A3_logistic_model, type = "response"))

# adding predicted diabetess classification
# and comparison with actual diabetes class
# and plot visualizing the results
A3_results_r_05 <-
  A3_classifications_and_plots(A3_pimatibble_pred_probs,
                               r = 0.5, A3_Glucose_range, A3_Age_range)

A3_results_r_02 <-
  A3_classifications_and_plots(A3_pimatibble_pred_probs,
                               r = 0.2, A3_Glucose_range, A3_Age_range)

A3_results_r_08 <-
  A3_classifications_and_plots(A3_pimatibble_pred_probs,
                            r = 0.8, A3_Glucose_range, A3_Age_range)

# adding decision boundary lines to Glucose vs Age plots
A3_boundary_05_plots <-
  A3_add_linear_boundary(A3_results_r_05, r = 0.5)

A3_boundary_02_plots <-
  A3_add_linear_boundary(A3_results_r_02, r = 0.2)

A3_boundary_08_plots <-
  A3_add_linear_boundary(A3_results_r_08, r = 0.8)

# basis function expansion
# sdding new features
A3_pimatibble_extended <-
  A3_pimatibble |>
  mutate(z1 = Glucose^4,
         z2 = Glucose^3*Age,
         z3 = Glucose^2*Age^2,
         z4 = Glucose*Age^3,
         z5 = Age^4)

A3_logistic_model_extended <-
  glm(Diabetes ~ Glucose + Age + z1 + z2 + z3 + z4 + z5,
      data = A3_pimatibble_extended, family = "binomial")

summary(A3_logistic_model_extended)

# coefficients (= coefficients in the probabilistic equation)

A3_logistic_extended_coef <- coef(A3_logistic_model_extended)

A3_theta0_ext <- A3_logistic_extended_coef["(Intercept)"]
A3_theta1_ext <- A3_logistic_extended_coef["Glucose"]
A3_theta2_ext <- A3_logistic_extended_coef["Age"]
A3_theta3_ext <- A3_logistic_extended_coef["z1"]
A3_theta4_ext <- A3_logistic_extended_coef["z2"]
A3_theta5_ext <- A3_logistic_extended_coef["z3"]
A3_theta6_ext <- A3_logistic_extended_coef["z4"]
A3_theta7_ext <- A3_logistic_extended_coef["z5"]


# adding predicted probabilities to data
A3_pimatibble_extended_pred_probs <-
  A3_pimatibble_extended |>
  mutate(Predicted_prob = predict(A3_logistic_model_extended, type = "response"))



A3_results_extended <-
  A3_classifications_and_plots(A3_pimatibble_extended_pred_probs,
                               r = 0.5,
                               A3_Glucose_range, A3_Age_range)

