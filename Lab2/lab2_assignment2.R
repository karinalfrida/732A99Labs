# Lab 2 Assignment 2
library(tidyverse)

# # bank client data:
# 1 - age (numeric)
# 2 - job : type of job (categorical: 'admin.','blue-
# collar','entrepreneur','housemaid','management','retired','self-
# employed','services','student','technician','unemployed','unknown')
# 3 - marital : marital status (categorical: 'divorced','married','single','unknown'; note:
#                                 'divorced' means divorced or widowed)
# 4 - education (categorical:
#                  'basic.4y','basic.6y','basic.9y','high.school','illiterate','professional.course','university.degree'
#                ,'unknown')
### !5 - default: has credit in default? (categorical: 'no','yes','unknown')
# 5 - balance:
# 6 - housing: has housing loan? (categorical: 'no','yes','unknown')
# 7 - loan: has personal loan? (categorical: 'no','yes','unknown')
#
# # rela!ted to the last contact of the current campaign:
### !8 - contact: contact communication type (categorical: 'cellular','telephone')
# 8 - contact: contact communication type (categorical: 'unknown', cellular','telephone')
# 9 - day: (numeric) date of month
### !10 - day_of_week: last contact day of the week (categorical: 'mon','tue','wed','thu','fri')
# 10 - month: last contact month of year (categorical: 'jan', 'feb', 'mar', ..., 'nov', 'dec')
# 11 - duration: last contact duration, in seconds (numeric). Important note: this attribute
# highly affects the output target (e.g., if duration=0 then y='no'). Yet, the duration is not
# known before a call is performed. Also, after the end of the call y is obviously known. Thus,
# this input should only be included for benchmark purposes and should be discarded if the
# intention is to have a realistic predictive model.
#
# # other attributes:
# 12 - campaign: number of contacts performed during this campaign and for this client
# (numeric, includes last contact)
# 13 - pdays: number of days that passed by after the client was last contacted from a
# previous campaign (numeric; 999 means client was not previously contacted)
# 14 - previous: number of contacts performed before this campaign and for this client
# (numeric)
### !15 - poutcome: outcome of the previous marketing campaign (categorical:
#                                                              'failure','nonexistent','success')
# 15 - poutcome: outcome of the previous marketing campaign (categorical:
#                                                             'unknown', 'failure','other','success')
# Output variable (target):
#   y - has the client subscribed a term deposit? (binary: 'yes','no')

################################################################################
# data import

# read data from file
a2_bankdata_raw <- read.csv2("data/bank-full.csv")

# any NA:s?
if (anyNA(a2_bankdata_raw)) {
  message("NA:s in table")
}

# checking categorical values
a2_bankdata_raw |>
  select(where(is.character) | where(is.factor)) |>
  map(unique)

a2_bankdata_raw |>
  select(where(is.character) | where(is.factor)) |>
  purrr::map(unique)

a2_bankdata <-
  a2_bankdata_raw |>
  # add observation ID
  mutate(ID = row_number(), .before = "age") |>
  # convert categorical features to factors
  mutate(
  job = factor(job, levels =c('admin.','blue-collar','entrepreneur','housemaid',
                                 'management','retired','self-employed','services',
                                 'student','technician','unemployed','unknown')),
  marital = factor(marital, levels = c('divorced','married','single','unknown')),
  default = factor(default, levels = c('no','yes','unknown')),
  housing = factor(housing, levels = c('no','yes','unknown')),
  loan = factor(loan, levels = c('no','yes','unknown')),
  contact = factor(contact, levels = c('unknown', 'cellular','telephone')),
  month = factor(month, levels = tolower(month.abb), ordered = TRUE),
  poutcome = factor(poutcome, levels = c('unknown','failure','other','success')),
  y = factor (y, levels = c('no','yes')))

################################################################################
# data overview, categorical features
a2_factor_features_with_y <-
  a2_bankdata |>
  select(where(is.factor)) |>
  pivot_longer(cols = -y,
             names_to = "Measure",
             values_to = "Value",
             values_transform = list(Value = as.character))

a2_factor_features_with_y |>
  filter(!(Measure == "job")) |>
  ggplot() +
  aes(x = Value, fill = y) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6)) +
  scale_fill_manual(labels = c("No", "Yes"),
                    values = c("darkorange", "purple")) +
  facet_wrap( ~ Measure, scales = "free_x", ncol = 2)

a2_factor_features_with_y |>
  filter(Measure == "job") |>
  ggplot() +
  aes(x = Value, fill = y) +
  geom_bar() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, angle=45, vjust = 0.6)) +
  scale_fill_manual(labels = c("No", "Yes"),
                    values = c("darkorange", "purple")) +
  facet_wrap( ~ Measure, scales = "free_x", ncol = 2)


