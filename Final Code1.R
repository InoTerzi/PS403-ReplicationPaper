library(knitr)
opts_chunk$set(echo = FALSE, 
               message = FALSE, 
               warning = FALSE, 
               results = "asis", 
               out.width = "100%")

# Load additional R packages here
library(tidyverse)
library(haven)
library(estimatr)
library(margins)
library(sandwich)
library(lmtest)
library(clarify)
library(ggplot2)
library(dplyr)
library(tinytable)
library(broom)
library(gt)
library(pscl)
library(magrittr)
library(marginaleffects)

###############################################################
############ -- Replication of Probit -- ######################
###############################################################

#Replication of armed UAV adoption model with probit and clustered standard errors

uav <- read_dta("UAV2014Datasetsup003.dta")

#I code armed programme as 1.

fit1 <- glm(armedprogram ~ terrdisputes + lnnterr5 + autocrat + democrat + lngdpcap + defense,
            data = uav, family = binomial(link = "probit"))

summary(fit1)
coefficients

# Clustered standard errors
cl_vcov1 <- sandwich::vcovCL(fit1, cluster = uav$cowcc)

coef_table_probit <- tidy(fit1) %>%
  # Replace default SEs with CLUSTERED SEs
  mutate(
    std.error = sqrt(diag(cl_vcov1)),
    statistic = estimate / std.error,
    p.value   = 2 * pnorm(abs(statistic), lower.tail = FALSE)
  ) %>%
  
  # Rename variables for nicer labels
  mutate(term = case_when(
    term == "terrdisputes" ~ "Territorial Disputes", 
    term == "lnnterr5" ~ "Terrorism", 
    term == "autocrat" ~ "Autocracy", 
    term == "democrat" ~ "Democracy",
    term == "lngdpcap" ~ "GDP per Capita",
    term == "defense" ~ "Alliance with UAV Provider",
    TRUE ~ term
  )) %>%
  
  # Format coefficient and SE as coef (SE)
  mutate(
    estimate  = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 2),
    p.value   = round(p.value, 3),
    coef_se   = paste0(estimate, " (", std.error, ")")
  ) %>%
  
  # Keep only columns we want in the table
  select(term, coef_se, statistic, p.value) %>%
  
  # Create the GT table
  gt() %>%
  tab_header(title = "Table 1: Probit AME with Clustered Standard Errors")

coef_table_probit

###############################################################
############ -- Modelling LPM -- ######################
###############################################################

#estimate Linear Probability Model for regime type and armed UAV program with same controls as paper
fit2 <- lm(
  armedprogram ~ democrat +autocrat + lnnterr5 + terrdisputes + lngdpcap + defense,
  data = uav, na.action = na.exclude
)

#Standard Errors clustered by country
cl_lpm <- vcovCL(fit2, cluster = uav$cowcc)

# Marginal effects for LPM
lpm_me <- slopes(
  fit2,
  vcov = cl_lpm,
  data = uav      
)

# Clustered standard errors
cl_vcov_lpm <- sandwich::vcovCL(fit2, cluster = uav$cowcc)

coef_table_lpm <- tidy(fit2) %>%
  # Replace default SEs with CLUSTERED SEs
  mutate(
    std.error = sqrt(diag(cl_vcov_lpm)),
    statistic = estimate / std.error,
    p.value   = 2 * pnorm(abs(statistic), lower.tail = FALSE)
  ) %>%
  
  # Rename variables for nicer labels
  mutate(term = case_when(
    term == "terrdisputes" ~ "Territorial Disputes", 
    term == "lnnterr5" ~ "Terrorism", 
    term == "autocrat" ~ "Autocracy", 
    term == "democrat" ~ "Democracy",
    term == "lngdpcap" ~ "GDP per Capita",
    term == "defense" ~ "Alliance with UAV Provider",
    TRUE ~ term
  )) %>%
  
  # Format coefficient and SE as coef (SE)
  mutate(
    estimate  = round(estimate, 3),
    std.error = round(std.error, 3),
    statistic = round(statistic, 2),
    p.value   = round(p.value, 3),
    coef_se   = paste0(estimate, " (", std.error, ")")
  ) %>%
  
  # Keep only columns we want in the table
  select(term, coef_se, statistic, p.value) %>%
  
  # Create the GT table
  gt() %>%
  tab_header(title = "Table 2: LPM Coefficients with Clustered Standard Errors")

coef_table_lpm

###############################################################
############ -- Comparing overall fit -- ######################
###############################################################


#calibration bins for the two models
calib_probit <- uav %>%
  filter(!is.na(armedprogram), !is.na(probit_pred)) %>%
  mutate(bin = ntile(probit_pred, 10)) %>%      # 10 quantile bins
  group_by(bin) %>%
  summarise(
    model     = "Probit",
    mean_pred = mean(probit_pred),
    actual    = mean(armedprogram),
    n         = n(),
    lower     = qbinom(.025, n, actual) / n,    # 95% CI lower
    upper     = qbinom(.975, n, actual) / n     # 95% CI upper
  )

calib_lpm <- uav %>%
  filter(!is.na(armedprogram), !is.na(lpm_pred_raw)) %>%
  mutate(bin = ntile(lpm_pred_raw, 10)) %>%
  group_by(bin) %>%
  summarise(
    model     = "LPM",
    mean_pred = mean(lpm_pred_raw),
    actual    = mean(armedprogram),
    n         = n(),
    lower     = qbinom(.025, n, actual) / n,
    upper     = qbinom(.975, n, actual) / n
  )


#combine calibration data
calib_both <- bind_rows(calib_probit, calib_lpm)
#plot with confidence intervals 

ggplot(calib_both, aes(x = mean_pred, y = actual, color = model)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.01) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +  # perfect calibration
  scale_colour_manual(values = c(
    "Probit" = "cyan4",
    "LPM"    = "grey"
  )) +
  labs(
    title = "Figure 1: Calibration Plot: Probit vs LPM (with 95% CIs)",
    x = "Mean Predicted Probability (per decile)",
    y = "Actual Probability of Armed UAV Program"
  ) +
  theme_minimal()

###############################################################
############ -- Comparing over regime type -- ######################
###############################################################

uav <- uav %>%
  mutate(
    regime_label = case_when(
      autocrat == 1 ~ "Autocracy",
      democrat == 1 ~ "Democracy",
      TRUE ~ "Mixed"
    ),
    regime_label = factor(regime_label,
                          levels = c("Autocracy", "Mixed", "Democracy"))
  )

regime_grid <- tibble(
  autocrat = c(1, 0, 0),
  democrat = c(0, 1, 0),
  regime_label = c("Autocracy", "Democracy", "Mixed")
)

# Fill in control variables with means (or medians)
regime_grid <- regime_grid %>%
  mutate(
    lnnterr5     = mean(uav$lnnterr5, na.rm = TRUE),
    terrdisputes = mean(uav$terrdisputes, na.rm = TRUE),
    lngdpcap     = mean(uav$lngdpcap, na.rm = TRUE),
    defense      = mean(uav$defense, na.rm = TRUE)
  )

#PREDICT FROM PROBIT AND LPM MODELS

# PROBIT predictions
pred_probit <- predictions(
  fit1,
  newdata = regime_grid,
  vcov = cl_vcov1
) %>% 
  mutate(model = "Probit")

# LPM predictions
pred_lpm <- predictions(
  fit2,
  newdata = regime_grid,
  vcov = cl_lpm  # your clustered vcov for LPM
) %>%
  mutate(model = "LPM")

#COMBINE

plot_df <- bind_rows(pred_probit, pred_lpm) %>%
  mutate(regime_label = factor(regime_label,
                               levels = c("Autocracy", "Mixed", "Democracy")))

#BOOTSTRAP PREDICTIONS

regime_grid <- tibble(
  autocrat = c(1, 0, 0),
  democrat = c(0, 1, 0),
  regime_label = c("Autocracy", "Democracy", "Mixed"),
  lnnterr5     = mean(uav$lnnterr5, na.rm = TRUE),
  terrdisputes = mean(uav$terrdisputes, na.rm = TRUE),
  lngdpcap     = mean(uav$lngdpcap, na.rm = TRUE),
  defense      = mean(uav$defense, na.rm = TRUE)
)

# DEFINE BOOTSTRAP FUNCTION


boot_diff_fun <- function(data, grid) {
  
  # Refit the probit and LPM models on bootstrap sample
  fit1_b <- glm(
    armedprogram ~ autocrat + democrat + lnnterr5 + terrdisputes +
      lngdpcap + defense,
    family = binomial(link = "probit"),
    data = data
  )
  
  fit2_b <- lm(
    armedprogram ~ autocrat + democrat + lnnterr5 + terrdisputes +
      lngdpcap + defense,
    data = data
  )
  
  # Predict using the same grid
  p_probit <- predictions(fit1_b, newdata = grid)$estimate
  p_lpm    <- predictions(fit2_b, newdata = grid)$estimate
  
  # Return the difference for each regime type
  return(p_probit - p_lpm)
}


set.seed(123)

B <- 1000

boot_preds <- map_dfr(1:B, function(b) {
  
  d <- uav %>% slice_sample(n = n(), replace = TRUE)
  
  # Refit models
  fit_probit <- glm(
    armedprogram ~ terrdisputes + lnnterr5 + autocrat + democrat + lngdpcap + defense,
    family = binomial(link = "probit"),
    data = d
  )
  
  fit_lpm <- lm(
    armedprogram ~ terrdisputes + lnnterr5 + autocrat + democrat + lngdpcap + defense,
    data = d
  )
  
  tibble(
    regime_label = regime_grid$regime_label,
    Probit = predict(fit_probit, regime_grid, type = "response"),
    LPM    = predict(fit_lpm, regime_grid),
    iter = b
  ) %>%
    pivot_longer(cols = c(Probit, LPM),
                 names_to = "model",
                 values_to = "estimate")
})
#SUMMARISE
plot_df <- boot_preds %>%
  group_by(model, regime_label) %>%
  summarise(
    estimate  = median(estimate),
    conf.low  = quantile(estimate, 0.025),
    conf.high = quantile(estimate, 0.975),
    .groups = "drop"
  )
#PLOT
ggplot(plot_df, aes(x = regime_label, y = estimate, color = model)) +
  geom_point(size = 3, position = position_dodge(width = .4)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = .4), width = 0.1) +
  geom_jitter(
    data = uav,
    aes(x = regime_label, y = armedprogram),
    inherit.aes = FALSE,
    width = 0.15,
    height = 0.02,
    alpha = 0.25,
    color = "black"
  ) +
  labs(
    x = "Regime Type",
    y = "Predicted Probability of Armed UAV Program",
    title = "Figure 2: Bootstrapped Predicted Probabilities by Regime Type"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Probit" = "cyan4", "LPM" = "grey"))


###############################################################
############ -- Comparing over terrorism level -- #############
###############################################################


#TERRORISM VALUES GRID
terror_range <- tibble(
  lnnterr5 = seq(min(uav$lnnterr5, na.rm = TRUE),
                 max(uav$lnnterr5, na.rm = TRUE),
                 length.out = 100),
  autocrat = mean(uav$autocrat),     # hold regime dummies at their mean
  democrat = mean(uav$democrat),
  terrdisputes = mean(uav$terrdisputes, na.rm = TRUE),
  lngdpcap = mean(uav$lngdpcap, na.rm = TRUE),
  defense = mean(uav$defense, na.rm = TRUE)
)

#MODEL PREDICTIONS

pred_probit_terror <- predictions(
  fit1,
  newdata = terror_range,
  vcov = cl_vcov1
) %>%
  mutate(model = "Probit")

pred_lpm_terror <- predictions(
  fit2,
  newdata = terror_range,
  vcov = cl_lpm
) %>%
  mutate(model = "LPM")

plot_df2 <- bind_rows(pred_probit_terror, pred_lpm_terror)

#PLOT

ggplot(plot_df2, aes(x = lnnterr5, y = estimate, color = model)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = model),
              alpha = 0.2, color = NA) +
  geom_jitter(data = uav, aes(x = lnnterr5, y = armedprogram),
              inherit.aes = FALSE, width = 0, height = 0.03,
              alpha = 0.25, colour = "black") +
  labs(
    x = "Terrorism (lnnterr5)",
    y = "Predicted Probability of Armed UAV Program",
    title = "Figure 3: Predicted Probabilities Across Terrorism Levels"
  ) +
  scale_color_manual(values = c("Probit" = "cyan4", "LPM" = "grey")) +
  scale_fill_manual(values = c("Probit" = "cyan4", "LPM" = "grey")) +
  theme_minimal()


###############################################################
############ -- Bootstrap terrorism -- #############
###############################################################
set.seed(123)

terror_grid <- tibble(
  lnnterr5 = seq(
    min(uav$lnnterr5, na.rm = TRUE),
    max(uav$lnnterr5, na.rm = TRUE),
    length.out = 100
  ),
  autocrat     = mean(uav$autocrat, na.rm = TRUE),
  democrat     = mean(uav$democrat, na.rm = TRUE),
  terrdisputes = mean(uav$terrdisputes, na.rm = TRUE),
  lngdpcap     = mean(uav$lngdpcap, na.rm = TRUE),
  defense      = mean(uav$defense, na.rm = TRUE)
)

# BOOTSTRAP FUNCTION - DIFFERENCE IN PREDICTIONS PER LEVEL
boot_terror_pred <- function(data, grid) {
  
  # Refit Probit
  fit1_b <- glm(
    armedprogram ~ autocrat + democrat + lnnterr5 +
      terrdisputes + lngdpcap + defense,
    family = binomial(link = "probit"),
    data = data
  )
  
  # Refit LPM
  fit2_b <- lm(
    armedprogram ~ autocrat + democrat + lnnterr5 +
      terrdisputes + lngdpcap + defense,
    data = data
  )
  
  # Predict on the same grid
  p_probit <- predictions(fit1_b, newdata = grid)$estimate
  p_lpm    <- predictions(fit2_b, newdata = grid)$estimate
  
  # Probit − LPM
  p_probit - p_lpm
}


#RUN 
B <- 1000

boot_mat_terror <- replicate(B, {
  samp <- uav[sample(nrow(uav), replace = TRUE), ]
  boot_terror_pred(samp, terror_grid)
})

#SUMMARISE
terror_boot_summary <- apply(
  boot_mat_terror,
  1,
  quantile,
  probs = c(.025, .50, .975)
)

terror_boot_summary <- as.data.frame(t(terror_boot_summary))
colnames(terror_boot_summary) <- c("Low95", "Median", "High95")
terror_boot_summary$lnnterr5 <- terror_grid$lnnterr5

#VISUALISE

ggplot(terror_boot_summary, aes(x = lnnterr5, y = Median)) +
  geom_line(color = "cyan4", linewidth = 1) +
  geom_ribbon(
    aes(ymin = Low95, ymax = High95),
    alpha = 0.25
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Figure 4: Model Disagreement Across Terrorism Levels",
    x = "Terrorism (ln scale)",
    y = "Difference in Predicted Probability (Probit − LPM)"
  ) +
  theme_minimal()

# Compute CI width at each terrorism value
terror_boot_summary <- terror_boot_summary %>%
  mutate(
    CI_width = High95 - Low95
  )

# Find the terrorism value with the largest uncertainty
max_uncertainty <- terror_boot_summary %>%
  slice_max(CI_width, n = 1)

max_uncertainty


#####last test#######
#bootstap function - prediction error curves
boot_terror_error <- function(data, x_vals) {
  
  # Refit models (same spec as above)
  fitP <- glm(
    armedprogram ~ autocrat + democrat + lnnterr5 +
      terrdisputes + lngdpcap + defense,
    family = binomial("probit"),
    data = data
  )
  
  fitL <- lm(
    armedprogram ~ autocrat + democrat + lnnterr5 +
      terrdisputes + lngdpcap + defense,
    data = data
  )
  
  # Prediction errors at observed data
  df <- data %>%
    mutate(
      err_probit = predict(fitP, type = "response") - armedprogram,
      err_lpm    = predict(fitL) - armedprogram
    )
  
  # Smooth conditional mean error vs terrorism
  fP <- loess(err_probit ~ lnnterr5, data = df, span = 0.75)
  fL <- loess(err_lpm    ~ lnnterr5, data = df, span = 0.75)
  
  tibble(
    lnnterr5 = x_vals,
    Probit   = predict(fP, newdata = data.frame(lnnterr5 = x_vals)),
    LPM      = predict(fL, newdata = data.frame(lnnterr5 = x_vals))
  )
}

#bootstrap
boot_error_curves <- replicate(B, {
  samp <- uav[sample(nrow(uav), replace = TRUE), ]
  boot_terror_error(samp, terror_grid$lnnterr5)
}, simplify = FALSE)

#summarise
terror_error_summary <- bind_rows(boot_error_curves, .id = "b") %>%
  pivot_longer(
    cols = c(Probit, LPM),
    names_to = "model",
    values_to = "error"
  ) %>%
  group_by(model, lnnterr5) %>%
  summarise(
    Low95  = quantile(error, 0.025, na.rm = TRUE),
    Median = quantile(error, 0.50,  na.rm = TRUE),
    High95 = quantile(error, 0.975, na.rm = TRUE),
    .groups = "drop"
  )

#visualise
ggplot(
  terror_error_summary,
  aes(x = lnnterr5, y = Median, color = model, fill = model)
) +
  geom_line(linewidth = 1) +
  geom_ribbon(
    aes(ymin = Low95, ymax = High95),
    alpha = 0.20,
    color = NA
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Figure 5: Bootstrapped Prediction Error Across Terrorism Levels",
    x = "Terrorism (ln scale)",
    y = "Predicted Probability − Observed Outcome"
  ) +
  scale_color_manual(values = c("Probit" = "cyan4", "LPM" = "grey40")) +
  scale_fill_manual(values = c("Probit" = "cyan4", "LPM" = "grey40")) +
  theme_minimal()

















