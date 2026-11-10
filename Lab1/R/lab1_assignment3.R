library(tidyverse)

# Data in pima-indians-diabetes.csv:
# 1. Number of times pregnant.
# 2. Plasma glucose concentration a 2 hours in an oral glucose tolerance test.
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

# create tibble with
pimatibble <-
  pimadata |>
  tibble() |>
  magrittr::set_colnames(c("Parity", "Glucose", "DBP", "Skinfold", "Insulin",
                         "BMI", "Pedigree", "Age", "Diabetes")) |>
  mutate(Diabetes = as.logical(Diabetes)) |>
  mutate(ID = row_number(), .before = Parity) |>
  select(ID, Age, Pedigree, Parity, BMI, Skinfold, DBP, Insulin, Glucose, Diabetes)

pimatibble_long<-
  pimatibble |>
  pivot_longer(cols = all_of(c("BMI", "Skinfold", "DBP", "Insulin", "Glucose", "Diabetes")),
               names_to = "Measure", values_to = "Value")

# 1. Scatterplot Glucose on Age

minmaxlist <-
  pimatibble |>
  select(Age, Glucose) |>
  map(
    \(x) tibble(min = min(x), max = max(x)))

plot_3_1 <-
  pimatibble |>
  ggplot() +
  aes(x = Age, y = Glucose, color = Diabetes) +
  geom_point(size = 1, alpha = 0.8) +
  theme_bw() +
  scale_colour_manual(labels = c("Non-diabetic", "Diabetic"),
                      values = c("darkmagenta", "darkorange")) +
  scale_x_continuous(
    limits = unlist(minmaxlist[["Age"]], use.names = FALSE),
    breaks = scales::breaks_width(10)) +
  scale_y_continuous(
    limits = unlist(minmaxlist[["Glucose"]], use.names = FALSE),
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(y = "Plasma glucose")

# 2. Logistic regression model

# Diabetes ~ Glucose + Age in a logistic regression model
# using glm()= with family = "binomial" since target has two levels

logistic_model1 <-
  glm(Diabetes ~ Glucose + Age, data = pimatibble, family = "binomial")

summary(logistic_model1)

# coefficients (= coefficients in the probabilistic equation)

logistic_coef1 <- coef(logistic_model1)

# set classification threshold
r1 <-  0.5

# adding predicted probabilities and their classification to the data tibble
pimatibble_with_predictions <-
  pimatibble |>
  mutate(Predicted_prob = predict(logistic_model1, type = "response"),
         Diabetes_prediction = ifelse(Predicted_prob > r1, TRUE, FALSE),
         Correct_pred = (Diabetes == Diabetes_prediction))

# confusion matrix
logistic_confusion1 <-
  table(Actual = pimatibble_with_predictions$Diabetes,
        Predicted = pimatibble_with_predictions$Diabetes_prediction)

# proportion correctly classified observations
correct_proportion <- sum(diag(logistic_confusion1))/sum(logistic_confusion1)

# training misclassification error
# = proportion of misclassified observations
training_misclass_error <- 1 - correct_proportion
# alternatively:
# training_misclass_error <-
#   mean(pimatibble$Diabetes_prediction != pimatibble$Diabetes)

# scatterplot colored by predictions (re-using minmaxlist)
plot_3_2 <-
  pimatibble_with_predictions |>
  ggplot() +
  aes(x = Age, y = Glucose, color = Diabetes_prediction) +
  geom_point(size = 1, alpha = 0.5) +
  theme_bw() +
  scale_colour_manual(labels = c("Non-diabetic", "Diabetic"),
                      values = c("darkmagenta", "darkorange")) +
  scale_x_continuous(
    limits = unlist(minmaxlist[["Age"]], use.names = FALSE),
    breaks = scales::breaks_width(10)) +
  scale_y_continuous(
    limits = unlist(minmaxlist[["Glucose"]], use.names = FALSE),
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(y = "Plasma glucose", color = "Prediction")

# scatterplot colored by correct or misclassfication (re-using minmaxlist)
plot_3_2x <-
  pimatibble_with_predictions |>
  ggplot() +
  aes(x = Age, y = Glucose, color = Correct_pred) +
  geom_point(size = 1, alpha = 0.8) +
  theme_bw() +
  scale_colour_manual(labels = c("Correct", "Misclassification"),
                      values = c("green", "red")) +
  scale_x_continuous(
    limits = unlist(minmaxlist[["Age"]], use.names = FALSE),
    breaks = scales::breaks_width(10)) +
  scale_y_continuous(
    limits = unlist(minmaxlist[["Glucose"]], use.names = FALSE),
    breaks = scales::breaks_width(10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(y = "Plasma glucose", color = "Classification result")
