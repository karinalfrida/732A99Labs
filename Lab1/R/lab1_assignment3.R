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

# read data
pimadata <- read.csv("data/pima-indians-diabetes.csv")

# any NA:s?
if (anyNA(pimadata)) {
  message("NA:s in table")
}

# create tibble with meaningful column names
pimatibble_raw <-
  pimadata |>
  tibble() |>
  magrittr::set_colnames(c("Parity", "Glucose", "DBP", "Skinfold", "Insulin",
                         "BMI", "Pedigree", "Age", "Diabetes")) |>
  mutate(Diabetes = as.logical(Diabetes)) |>
  mutate(ID = row_number(), .before = Parity) |>
  select(ID, Age, Pedigree, Parity, BMI, Skinfold, DBP, Insulin, Glucose, Diabetes)

# long form tibble
pimatibble_long_raw <-
  pimatibble_raw |>
  pivot_longer(-c(ID,Diabetes), names_to = "Measure", values_to = "Value")

# data check

pimatibble_long_raw |>
  ggplot() +
  aes(x=Value, fill = Diabetes) +
  geom_histogram(bins = 30) +
  scale_fill_manual(labels = c("Non-diabetic", "Diabetic"),
                      values = c("darkmagenta", "darkorange")) +
  facet_wrap(~ Measure, scales = "free", nrow = 2) +
  theme_bw() +
  theme(legend.position = "bottom")

# checking exact range for Age, Glucose, and Diabetes
pimatibble_raw |>
  select(c(Age, Glucose, Diabetes)) |>
  map(\(x) range(x))

# removing cases with Glucose == 0
pimatibble <-
  pimatibble_raw |>
  filter(Glucose != 0)

# long form tibble from cleaned data
pimatibble_long<-
  pimatibble |>
  pivot_longer(-c(ID,Diabetes), names_to = "Measure", values_to = "Value")

# saving adjusted ranges
minmaxlist <-
  pimatibble |>
  select(Age, Glucose) |>
  map(\(x) tibble(min = min(x), max = max(x)))

Age_range <- unlist(minmaxlist[["Age"]])
Glucose_range <- unlist(minmaxlist[["Glucose"]], use.names = FALSE)


# 1. Scatterplot Glucose vs Age

Glucose_vs_Age_scatter <-
  pimatibble |>
  ggplot() +
  aes(x = Age, y = Glucose, color = Diabetes) +
  geom_point(size = 1, alpha = 0.8) +
  theme_bw() +
  scale_colour_manual(labels = c("Non-diabetic", "Diabetic"),
                      values = c("darkmagenta", "darkorange")) +
  scale_x_continuous(
    limits = Age_range,
    breaks = scales::breaks_width(width=10)) + # equally spaced breaks
  scale_y_continuous(
    limits = Glucose_range,
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(y = "Plasma glucose")

# 2. Logistic regression model

# Diabetes ~ Glucose + Age in a logistic regression model
# using glm()= with family = "binomial" since target has two levels

logistic_model <-
  glm(Diabetes ~ Glucose + Age, data = pimatibble, family = "binomial")

summary(logistic_model)

# coefficients (= coefficients in the probabilistic equation)

logistic_coef <- coef(logistic_model)

theta0 <- logistic_coef["(Intercept)"]
theta1 <- logistic_coef["Glucose"]
theta2 <- logistic_coef["Age"]

# adding predicted probabilities to data
pimatibble_pred_probs <-
  pimatibble |>
  mutate(Predicted_prob = predict(logistic_model, type = "response"))

################################################################################
# helper function for classifications and plots
################################################################################
classifications_and_plots <- function(data_pred_prob, # data + predicted probabilities
                                theta0, theta1, theta2, # coefficients
                                r, # classification threshold
                                glucose_range, age_range # data ranges
                                ){

# adding prediction classifications to the data tibble
data_with_predictions <-
  data_pred_prob |>
  mutate(Diabetes_prediction = ifelse(Predicted_prob > r, TRUE, FALSE),
         Correct_pred = (Diabetes == Diabetes_prediction))

# confusion matrix
logistic_confusion <-
  table(Actual = data_with_predictions$Diabetes,
        Predicted = data_with_predictions$Diabetes_prediction)

# proportion correctly classified observations
correct_proportion <-
  sum(diag(logistic_confusion))/sum(logistic_confusion)

# training misclassification error
# = proportion of misclassified observations
training_misclass_error <-
  1 - correct_proportion

# decision boundary line intercept and slope

boundary_intercept <-  (log(r/(1-r))-theta0)/theta1
boundary_slope <-  -theta2/theta1

# scatterplot colored by predictions
pred_plot <-
  data_with_predictions |>
  ggplot() +
  aes(x = Age, y = Glucose, color = Diabetes_prediction) +
  geom_point(size = 1, shape = 8) +
  theme_bw() +
  scale_colour_manual(labels = c("Non-diabetic", "Diabetic"),
                      values = c("darkmagenta", "darkorange")) +
  scale_x_continuous(
    limits = age_range,
    breaks = scales::breaks_width(10)) +
  scale_y_continuous(
    limits = glucose_range,
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(y = "Plasma glucose", color = "Prediction")

# scatterplot colored by correct or misclassfication
class_plot <-
  data_with_predictions |>
  ggplot() +
  aes(x = Age, y = Glucose, color = Correct_pred, shape = Correct_pred) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(
    name = "Classification result",
    values = c(`TRUE` = "green", `FALSE` = "red"),
    labels = c(`TRUE` = "Correct", `FALSE` = "Misclassification")
  ) +
  scale_shape_manual(
    name = "Classification result",
    values = c(`TRUE` = 18, `FALSE` = 4),
    labels = c(`TRUE` = "Correct", `FALSE` = "Misclassification")
  ) +
  scale_x_continuous(
    limits = age_range,
    breaks = scales::breaks_width(10)) +
  scale_y_continuous(
    limits = glucose_range,
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(y = "Plasma glucose", color = "Classification result")+
  geom_abline(
    intercept = boundary_intercept,
    slope = boundary_slope,
    color = "black",
    linewidth = 1
  )

return(list(data_with_predictions = data_with_predictions,
            confusion_matrix = logistic_confusion,
            correct_proportion = correct_proportion,
            training_misclass_error = training_misclass_error,
            boundary_intercept = boundary_intercept,
            boundary_slope = boundary_slope,
            pred_plot = pred_plot,
            class_plot = class_plot))
}


add_boundary <-  function(results_list){
  pred_plot_with_line <-
    results_list[["pred_plot"]] +
    geom_abline(
      intercept = results_list[["boundary_intercept"]],
      slope = results_list[["boundary_slope"]],
      color = "black",
      linewidth = 1
    )
  class_plot_with_line <-
    results_list[["class_plot"]] +
    geom_abline(
      intercept = results_list[["boundary_intercept"]],
      slope = results_list[["boundary_slope"]],
      color = "black",
      linewidth = 1
    )
  return(list(pred_plot_with_line=pred_plot_with_line,
              class_plot_with_line=class_plot_with_line))
}



################################################################################

results_r_05 <-
  classifications_and_plots(pimatibble_pred_probs,
                            theta0, theta1, theta2, r = 0.5,
                            Glucose_range, Age_range)

results_r_02 <-
  classifications_and_plots(pimatibble_pred_probs,
                            theta0, theta1, theta2, r = 0.2,
                            Glucose_range, Age_range)

results_r_08 <-
  classifications_and_plots(pimatibble_pred_probs,
                            theta0, theta1, theta2, r = 0.8,
                            Glucose_range, Age_range)

boundary_05_plots <-
  add_boundary(results_r_05)

boundary_02_plots <-
  add_boundary(results_r_02)

boundary_08_plots <-
  add_boundary(results_r_08)
