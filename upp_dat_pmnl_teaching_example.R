############################################################## 
# UPP DAT Teaching Example 
# 
# Goal: 
# Estimate an individual patient's preferences from a series 
# of DCE choice tasks and generate a personalised report. 
# 
# Workflow: 
# 
# Choice Tasks 
# ↓ 
# Patient Responses 
# ↓ 
# pMNL Model 
# ↓ 
# Preference Coefficients 
# ↓ 
# Relative Importance Scores 
# ↓ 
# Patient-Friendly Report 
##############################################################

library(dplyr)
library(tibble)
library(tidyr)
library(ggplot2)
library(readr)

############################################################## 
# STEP 1: DEFINE THE ATTRIBUTES 
# 
# These represent the features of pain-management plans that 
# patients may care about. 
# 
# The model will estimate how important each feature is. 
##############################################################

attributes <- c(
  "otc_medicine",
  "prescription_medicine",
  "exercise_plan",
  "coping_strategies",
  "better_average_day",
  "fewer_bad_days",
  "better_activities",
  "no_side_effects"
)

attribute_labels <- c(
  otc_medicine = "Use over-the-counter medicines",
  prescription_medicine = "Use prescription medicines",
  exercise_plan = "Carry out an exercise plan",
  coping_strategies = "Receive advice on other coping strategies",
  better_average_day = "Feel better on an average day",
  fewer_bad_days = "Have fewer bad days",
  better_activities = "Be able to do more activities",
  no_side_effects = "Likely have no side effects"
)

attribute_group <- c(
  otc_medicine = "Actions",
  prescription_medicine = "Actions",
  exercise_plan = "Actions",
  coping_strategies = "Actions",
  better_average_day = "Outcomes",
  fewer_bad_days = "Outcomes",
  better_activities = "Outcomes",
  no_side_effects = "Outcomes"
)

############################################################## 
# STEP 2: DEFINE THE CHOICE TASKS 
# 
# Each row represents one treatment option. 
# 
# Task 1 has: 
# Option A 
# Option B 
# 
# Task 2 has: 
# Option A 
# Option B 
# 
# etc. 
# 
# The numbers indicate whether that feature is present (1) 
# or absent (0). 
# 
# Example: 
# 
# exercise_plan = 1 
# 
# means the treatment includes an exercise plan. 
##############################################################

design <- tribble(
  ~task, ~alt, ~otc_medicine, ~prescription_medicine, ~exercise_plan, ~coping_strategies, ~better_average_day, ~fewer_bad_days, ~better_activities, ~no_side_effects,
  1, "A", 0,1,0,0, 1,0,1,1,
  1, "B", 1,0,0,1, 1,1,1,1,
  2, "A", 1,0,0,1, 0,1,0,0,
  2, "B", 1,0,0,0, 1,0,1,1,
  3, "A", 1,1,1,0, 0,1,1,1,
  3, "B", 0,0,1,1, 1,0,0,0,
  4, "A", 1,0,0,0, 1,0,0,1,
  4, "B", 0,1,0,1, 0,0,1,0,
  5, "A", 0,1,0,0, 0,1,1,0,
  5, "B", 0,1,1,0, 1,1,0,1,
  6, "A", 0,0,1,1, 1,0,1,0,
  6, "B", 1,0,0,0, 0,1,1,0,
  7, "A", 0,1,1,1, 1,1,0,1,
  7, "B", 1,1,1,0, 0,0,1,0,
  8, "A", 1,1,0,0, 1,0,0,0,
  8, "B", 0,0,1,0, 0,1,1,1,
  9, "A", 0,0,1,0, 1,1,1,0,
  9, "B", 0,1,0,1, 0,1,0,1,
  10, "A", 1,0,0,1, 1,1,0,1,
  10, "B", 1,0,0,0, 1,0,0,0,
  11, "A", 0,0,1,0, 0,1,0,1,
  11, "B", 1,1,1,1, 1,1,1,0,
  12, "A", 1,1,1,1, 0,0,1,1,
  12, "B", 0,1,1,0, 1,1,0,0
)

############################################################## 
# STEP 3: RECORD PATIENT CHOICES 
# 
# For each task the patient chooses: 
# 
# "A" or "B" 
# 
# These choices are the only information used to estimate 
# their preferences. 
##############################################################

participant_choices <- c(
  "A", "A", "A", "A", "A", "A",
  "A", "A", "A", "A", "A", "A"
)

responses <- tibble(
  task = 1:12,
  choice = toupper(participant_choices)
)

############################################################## 
# STEP 4: PREPARE THE DATA 
# 
# The model compares the attributes of Option A and Option B. 
# 
# For each task we calculate: 
# 
# Attribute Difference 
# 
# = Option A - Option B 
# 
# This converts the choice experiment into a format that can 
# be analysed by the multinomial logit model. 
##############################################################

make_model_data <- function(design, responses, attributes) {
  wide <- design %>%
    pivot_wider(
      id_cols = task,
      names_from = alt,
      values_from = all_of(attributes),
      names_sep = "_"
    ) %>%
    left_join(responses, by = "task") %>%
    mutate(y = if_else(choice == "A", 1, 0))
  
  X_A <- as.matrix(wide[, paste0(attributes, "_A")])
  X_B <- as.matrix(wide[, paste0(attributes, "_B")])
  Xdiff <- X_A - X_B
  colnames(Xdiff) <- attributes
  
  list(y = wide$y, X = Xdiff, wide = wide)
}

############################################################## 
# STEP 5: ESTIMATE THE pMNL MODEL 
# 
# pMNL = Penalised Multinomial Logit 
# 
# Why use a penalty? 
# 
# We only have 12 choice tasks. 
# 
# Without a penalty the model can produce unstable or 
# unrealistically large coefficients. 
# 
# The penalty acts like a "safety brake" that prevents the # model becoming overconfident when data are limited. 
##############################################################

fit_pmnl <- function(X, y, ridge = 1e-7) {
  neg_pen_ll <- function(beta) {
    eta <- as.vector(X %*% beta)
    ll <- sum(y * eta - log1p(exp(eta)))
    p <- plogis(eta)
    w <- p * (1 - p)
    I <- t(X) %*% (X * w) + diag(ridge, ncol(X))
    log_det_I <- as.numeric(determinant(I, logarithm = TRUE)$modulus)
    -(ll + 0.5 * log_det_I)
  }
  
  fit <- optim(
    par = rep(0, ncol(X)),
    fn = neg_pen_ll,
    method = "BFGS",
    control = list(maxit = 5000, reltol = 1e-10)
  )
  
  beta <- fit$par
  names(beta) <- colnames(X)
  
  list(beta = beta, convergence = fit$convergence, value = fit$value)
}

make_report <- function(beta) {
  report <- tibble(
    attribute = names(beta),
    group = unname(attribute_group[names(beta)]),
    label = unname(attribute_labels[names(beta)]),
    beta = as.numeric(beta)
  )
  
  report %>%
    group_by(group) %>%
    mutate(
      max_abs_beta = max(abs(beta)),
      directional_score = if (max_abs_beta[1] == 0) {
        rep(0, n())
      } else {
        100 * beta / max_abs_beta[1]
      },
      relative_importance = abs(directional_score),
      interpretation = case_when(
        beta > 0 ~ "1-coded level preferred",
        beta < 0 ~ "0-coded level preferred",
        TRUE ~ "No clear preference"
      )
    ) %>%
    ungroup() %>%
    arrange(group, desc(relative_importance))
}

plot_group <- function(report, group_name, title) {
  dat <- report %>%
    filter(group == group_name) %>%
    arrange(relative_importance)
  
  ggplot(dat, aes(x = reorder(label, relative_importance), y = relative_importance)) +
    geom_col() +
    coord_flip() +
    labs(
      title = title,
      x = NULL,
      y = "Relative importance"
    ) +
    theme_minimal(base_size = 12)
}

model_data <- make_model_data(design, responses, attributes)
pmnl <- fit_pmnl(model_data$X, model_data$y)
report <- make_report(pmnl$beta)

############################################################## 
# STEP 6: INTERPRET THE COEFFICIENTS 
# 
# Positive coefficient: 
# 
# Patient prefers the "1" level 
# 
# Negative coefficient: 
# 
# Patient prefers the "0" level 
# 
# Larger magnitude: 
# 
# Stronger preference 
# 
# Example: 
# 
# better_activities = 2.1 
# 
# indicates that being able to do more activities strongly 
# influenced the patient's choices. 
##############################################################

print(pmnl$beta)

############################################################## 
# STEP 7: CREATE A PATIENT-FRIENDLY REPORT 
# 
# Raw coefficients are difficult to interpret. 
# 
# Therefore we convert them into relative importance scores. 
# 
# The most important attribute is scaled to 100. 
# 
# All other attributes are expressed relative to it. 
##############################################################

print(report)

############################################################## 
# STEP 8: VISUALISE THE RESULTS 
# 
# The final plots show: 
# 
# 1. Preferred management actions 
# 2. Preferred outcomes 
# 
# These outputs can then be discussed during a shared # decision-making consultation. 
##############################################################

actions_plot <- plot_group(report, "Actions", "Actions you prefer")
outcomes_plot <- plot_group(report, "Outcomes", "Outcomes that matter most to you")

print(actions_plot)
print(outcomes_plot)

write_csv(report, "UPP_individual_preference_report.csv")

ggsave("UPP_actions_plot.png", actions_plot, width = 7, height = 4, dpi = 300)
ggsave("UPP_outcomes_plot.png", outcomes_plot, width = 7, height = 4, dpi = 300)