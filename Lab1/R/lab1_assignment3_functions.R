################################################################################
# function definitions

################################################################################
#' A3_classifications_and_plots
#' Adds predictions based on provided predicted probabilities
#' and decision threshold, and creates a plot colored by diabetes prediction
#' and a plot colored by the predicion being correct or not,
#' the latter is extra so not presented in report.
#'
#' @param data_pred_prob A tibble with data and predicted probabilities
#' @param r Classification threshold
#' @param glucose_range Numerical vector with min and max of Glucose values
#' @param age_range Numerical vector with min and max of Age values
#'
#' @returns A list of:
#' data_with_predictions
#'    = data where Diabetes_prediction and Correct_prediction
#'    columns (both logical) are added
#' confusion_matrix
#'    = confusion matrix for Actual vs Predicted diabetes
#' correct_proportion
#'    = Proportion of correctly classified observations
#' training_misclass_error
#'    = Proportion of misclassified observations
#' pred_plot
#'    = Plot of Glucose vs Age, colored by Diabetes_predicion
#' class_plot
#'    = Plot of Glucose vs Age, colored by Correct_pred
#'
A3_classifications_and_plots <- function(data_pred_prob, # data + predicted probabilities
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

  # scatterplot colored by predictions
  pred_plot <-
    data_with_predictions |>
    ggplot() +
    aes(x = Age, y = Glucose, color = Diabetes_prediction) +
    geom_point(size = 0.3, shape = 8) +
    theme_bw() +
    theme_bw() +
    theme(legend.position = "top") +
    scale_colour_manual(labels = c("Non-diabetic", "Diabetic"),
                        values = c("darkmagenta", "darkorange")) +
    scale_x_continuous(
      limits = age_range + c(-5, +5),
      breaks = scales::breaks_width(10)) +
    scale_y_continuous(
      limits = glucose_range + c(-5, +5),
      breaks = scales::breaks_width(10),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(y = "Plasma glucose", color = "Diabetes prediction")

  # scatterplot colored by correct or misclassfication
  class_plot <-
    data_with_predictions |>
    ggplot() +
    aes(x = Age, y = Glucose, color = Correct_pred, shape = Correct_pred) +
    geom_point(size = 0.3) +
    theme_bw() +
    theme(legend.position = "top") +
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
      limits = age_range + c(-5, +5),
      breaks = scales::breaks_width(10)) +
    scale_y_continuous(
      limits = glucose_range + c(-5, +5),
      breaks = scales::breaks_width(10),
      expand = expansion(mult = c(0, 0.05))
    )

  return(list(data_with_predictions = data_with_predictions,
              confusion_matrix = logistic_confusion,
              correct_proportion = correct_proportion,
              training_misclass_error = training_misclass_error,
              pred_plot = pred_plot,
              class_plot = class_plot))
}

################################################################################
#' A3_calculate_linear_boundary
#' Calculating a linear boundary intercept and slope
#'
#' @param theta0 Fitted intercept
#' @param theta1 Fitted coefficient for Glucose
#' @param theta2 Fitted coefficient for Age
#'
#' @returns A list with boundary intercept and slope
#'
A3_calculate_linear_boundary <- function(theta0, theta1, theta2, r){

  # decision boundary line intercept and slope
  boundary_intercept <-  (log(r/(1-r))-theta0)/theta1
  boundary_slope <-  -theta2/theta1

  return(list(boundary_intercept = boundary_intercept,
              boundary_slope =  boundary_slope))
}

################################################################################
#' A3_add_linear_boundary
#' Adds a decision boundary line to the provided plots
#'
#' @param results_list List returned by
#' A3_classifications_and_plots
#'
#' @returns A list of two plots,
#' one colored by diabetes prediction, and
#' one colored by the predicion being correct or not
#'
A3_add_linear_boundary <-  function(results_list, r ){
  # calculate intercept and slope for the straight line
  boundary <-
    A3_calculate_linear_boundary(A3_theta0, A3_theta1, A3_theta2, r)

  # add line to prediction plot
  pred_plot_with_line <-
    results_list[["pred_plot"]] +
    geom_abline(
      intercept = boundary[["boundary_intercept"]],
      slope = boundary[["boundary_slope"]],
      color = "black",
      linewidth = 1
    )

  # add line to classification correctness plot
  class_plot_with_line <-
    results_list[["class_plot"]] +
    geom_abline(
      intercept = boundary[["boundary_intercept"]],
      slope = boundary[["boundary_slope"]],
      color = "black",
      linewidth = 1
    )
  return(list(pred_plot_with_line=pred_plot_with_line,
              class_plot_with_line=class_plot_with_line))
}

################################################################################
