get_min_metric_by_name <- function(name_val, df, metric) {
  df_name <- df %>% filter(Name == name_val)
  
  metric_sym <- ensym(metric)
  
  # Group and summarize by all parameter combinations
  summarize_by_params <- function(df, group_param, combo_format) {
    df %>%
      group_by(a, b, kappa, gamma) %>%
      summarize(Metric = mean(!!metric_sym, na.rm = TRUE), .groups = "drop") %>%
      mutate(Combination = combo_format(a, b, kappa, gamma)) %>%
      select({{ group_param }}, Combination, Metric) %>%
      pivot_wider(names_from = Combination, values_from = Metric, values_fn = mean, values_fill = NA)
  }
  
  # Define label formats
  combo_a <- function(a, b, kappa, gamma) paste0("b", b, "_k", kappa, "_g", gamma)
  combo_b <- function(a, b, kappa, gamma) paste0("a", a, "_k", kappa, "_g", gamma)
  combo_kappa <- function(a, b, kappa, gamma) paste0("a", a, "_b", b, "_g", gamma)
  combo_gamma <- function(a, b, kappa, gamma) paste0("a", a, "_b", b, "_k", kappa)
  
  # Summaries
  summary_a <- summarize_by_params(df_name, a, combo_a)
  summary_b <- summarize_by_params(df_name, b, combo_b)
  summary_kappa <- summarize_by_params(df_name, kappa, combo_kappa)
  summary_gamma <- summarize_by_params(df_name, gamma, combo_gamma)
  
  # Add Min and Final_par columns
  add_min_cols <- function(df, group_var, suffix) {
    df %>%
      rowwise() %>%
      mutate(
        !!paste0("Min_", suffix) := min(c_across(-{{ group_var }}), na.rm = TRUE),
        Final_par = names(which.min(replace(c_across(-{{ group_var }}), is.na(c_across(-{{ group_var }})), Inf)))
      ) %>%
      ungroup()
  }
  
  summary_a <- add_min_cols(summary_a, a, "a")
  summary_b <- add_min_cols(summary_b, b, "b")
  summary_kappa <- add_min_cols(summary_kappa, kappa, "kappa")
  summary_gamma <- add_min_cols(summary_gamma, gamma, "gamma")
  
  # Extract min metric values
  min_vals <- c(
    a     = min(summary_a[[paste0("Min_", "a")]], na.rm = TRUE),
    b     = min(summary_b[[paste0("Min_", "b")]], na.rm = TRUE),
    kappa = min(summary_kappa[[paste0("Min_", "kappa")]], na.rm = TRUE),
    gamma = min(summary_gamma[[paste0("Min_", "gamma")]], na.rm = TRUE)
  )
  
  final_param <- names(min_vals)[which.min(min_vals)]
  
  list(
    min_vector = c(min_vals, Final_par = final_param),
    summary_a = summary_a,
    summary_b = summary_b,
    summary_kappa = summary_kappa,
    summary_gamma = summary_gamma
  )
}

###### RMSE RV ###########
# Run for all Names
Baseline_results_list_RMSE_RV <- lapply(
  unique(results$Name),
  get_min_metric_by_name,
  df = results,
  metric = RMSE_RV
)

# Combine min values
Baseline_min_df_RMSE_RV <- do.call(rbind, lapply(Baseline_results_list_RMSE_RV, function(x) x$min_vector[1:4]))
rownames(Baseline_min_df_RMSE_RV) <- unique(results$Name)

Baseline_min_final_RMSE_RV <- as.data.frame(Baseline_min_df_RMSE_RV) %>%
  mutate(
    final = pmin(a, b, kappa, gamma, na.rm = TRUE),
    Final_par = sapply(Baseline_results_list_RMSE_RV, function(x) x$min_vector["Final_par"])
  )

# Extract summary tables
Baseline_summary_a_RMSE_RV     <- lapply(Baseline_results_list_RMSE_RV, function(x) x$summary_a)
Baseline_summary_b_RMSE_RV     <- lapply(Baseline_results_list_RMSE_RV, function(x) x$summary_b)
Baseline_summary_kappa_RMSE_RV <- lapply(Baseline_results_list_RMSE_RV, function(x) x$summary_kappa)
Baseline_summary_gamma_RMSE_RV <- lapply(Baseline_results_list_RMSE_RV, function(x) x$summary_gamma)

# Optional view
View(Baseline_summary_a_RMSE_RV[[1]])
View(Baseline_summary_b_RMSE_RV[[1]])
View(Baseline_summary_kappa_RMSE_RV[[1]])
View(Baseline_summary_gamma_RMSE_RV[[1]])

library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)

# Prepare the summary tables
param_summaries <- list(
  a     = Baseline_summary_a_RMSE_RV[[1]] %>% select(Parameter = a, RMSE = Min_a),
  b     = Baseline_summary_b_RMSE_RV[[1]] %>% select(Parameter = b, RMSE = Min_b),
  kappa = Baseline_summary_kappa_RMSE_RV[[1]] %>% select(Parameter = kappa, RMSE = Min_kappa),
  gamma = Baseline_summary_gamma_RMSE_RV[[1]] %>% select(Parameter = gamma, RMSE = Min_gamma)
)

# Combine into one data frame
plot_data <- bind_rows(
  lapply(names(param_summaries), function(param) {
    param_summaries[[param]] %>%
      mutate(Variable = param)
  }),
  .id = "ID"
)

# Define reference lines for each variable
reference_lines <- tibble(
  Variable = c("a", "b", "kappa", "gamma"),
  Reference = c(0.7, 0.7, 4, 4)
)

# Merge to include reference line per facet
plot_data <- plot_data %>%
  left_join(reference_lines, by = "Variable")

# Plot all in a single faceted plot
Baseline_RMSE_RV <- ggplot(plot_data, aes(x = Parameter, y = RMSE)) +
  geom_line(color = "steelblue") +
  geom_vline(aes(xintercept = Reference), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Minimum RMSE RV vs Each Parameter",
    x = "Parameter Value",
    y = "Minimum RMSE"
  ) +
  theme_minimal()

###### RMSE HV ###########
# Run for all Names
Baseline_results_list_RMSE_HV <- lapply(
  unique(results$Name),
  get_min_metric_by_name,
  df = results,
  metric = RMSE_HV
)

# Combine min values
Baseline_min_df_RMSE_HV <- do.call(rbind, lapply(Baseline_results_list_RMSE_HV, function(x) x$min_vector[1:4]))
rownames(Baseline_min_df_RMSE_HV) <- unique(results$Name)

Baseline_min_final_RMSE_HV <- as.data.frame(Baseline_min_df_RMSE_HV) %>%
  mutate(
    final = pmin(a, b, kappa, gamma, na.rm = TRUE),
    Final_par = sapply(Baseline_results_list_RMSE_HV, function(x) x$min_vector["Final_par"])
  )

# Extract summary tables
Baseline_summary_a_RMSE_HV     <- lapply(Baseline_results_list_RMSE_HV, function(x) x$summary_a)
Baseline_summary_b_RMSE_HV     <- lapply(Baseline_results_list_RMSE_HV, function(x) x$summary_b)
Baseline_summary_kappa_RMSE_HV <- lapply(Baseline_results_list_RMSE_HV, function(x) x$summary_kappa)
Baseline_summary_gamma_RMSE_HV <- lapply(Baseline_results_list_RMSE_HV, function(x) x$summary_gamma)

# Optional view
View(Baseline_summary_a_RMSE_HV[[1]])
View(Baseline_summary_b_RMSE_HV[[1]])
View(Baseline_summary_kappa_RMSE_HV[[1]])
View(Baseline_summary_gamma_RMSE_HV[[1]])

# Prepare the summary tables
param_summaries <- list(
  a     = Baseline_summary_a_RMSE_HV[[1]] %>% select(Parameter = a, RMSE = Min_a),
  b     = Baseline_summary_b_RMSE_HV[[1]] %>% select(Parameter = b, RMSE = Min_b),
  kappa = Baseline_summary_kappa_RMSE_HV[[1]] %>% select(Parameter = kappa, RMSE = Min_kappa),
  gamma = Baseline_summary_gamma_RMSE_HV[[1]] %>% select(Parameter = gamma, RMSE = Min_gamma)
)

# Combine into one data frame
plot_data <- bind_rows(
  lapply(names(param_summaries), function(param) {
    param_summaries[[param]] %>%
      mutate(Variable = param)
  }),
  .id = "ID"
)

# Define reference lines for each variable
reference_lines <- tibble(
  Variable = c("a", "b", "kappa", "gamma"),
  Reference = c(0.7, 0.7, 4, 4)
)

# Merge to include reference line per facet
plot_data <- plot_data %>%
  left_join(reference_lines, by = "Variable")

# Plot all in a single faceted plot
Baseline_RMSE_HV <- ggplot(plot_data, aes(x = Parameter, y = RMSE)) +
  geom_line(color = "steelblue") +
  geom_vline(aes(xintercept = Reference), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Minimum RMSE HV vs Each Parameter",
    x = "Parameter Value",
    y = "Minimum RMSE HV"
  ) +
  theme_minimal()


###### MAE RV ###########
# Run for all Names
Baseline_results_list_MAE_RV <- lapply(
  unique(results$Name),
  get_min_metric_by_name,
  df = results,
  metric = MAE_RV
)

# Combine min values
Baseline_min_df_MAE_RV <- do.call(rbind, lapply(Baseline_results_list_MAE_RV, function(x) x$min_vector[1:4]))
rownames(Baseline_min_df_MAE_RV) <- unique(results$Name)

Baseline_min_final_MAE_RV <- as.data.frame(Baseline_min_df_MAE_RV) %>%
  mutate(
    final = pmin(a, b, kappa, gamma, na.rm = TRUE),
    Final_par = sapply(Baseline_results_list_MAE_RV, function(x) x$min_vector["Final_par"])
  )

# Extract summary tables
Baseline_summary_a_MAE_RV     <- lapply(Baseline_results_list_MAE_RV, function(x) x$summary_a)
Baseline_summary_b_MAE_RV     <- lapply(Baseline_results_list_MAE_RV, function(x) x$summary_b)
Baseline_summary_kappa_MAE_RV <- lapply(Baseline_results_list_MAE_RV, function(x) x$summary_kappa)
Baseline_summary_gamma_MAE_RV <- lapply(Baseline_results_list_MAE_RV, function(x) x$summary_gamma)

# Optional view
# View(Baseline_summary_a_MAE_RV[[1]])
# View(Baseline_summary_b_MAE_RV[[1]])
# View(Baseline_summary_kappa_MAE_RV[[1]])
# View(Baseline_summary_gamma_MAE_RV[[1]])

# Prepare the summary tables
param_summaries <- list(
  a     = Baseline_summary_a_MAE_RV[[1]] %>% select(Parameter = a, RMSE = Min_a),
  b     = Baseline_summary_b_MAE_RV[[1]] %>% select(Parameter = b, RMSE = Min_b),
  kappa = Baseline_summary_kappa_MAE_RV[[1]] %>% select(Parameter = kappa, RMSE = Min_kappa),
  gamma = Baseline_summary_gamma_MAE_RV[[1]] %>% select(Parameter = gamma, RMSE = Min_gamma)
)

# Combine into one data frame
plot_data <- bind_rows(
  lapply(names(param_summaries), function(param) {
    param_summaries[[param]] %>%
      mutate(Variable = param)
  }),
  .id = "ID"
)

# Define reference lines for each variable
reference_lines <- tibble(
  Variable = c("a", "b", "kappa", "gamma"),
  Reference = c(0.7, 0.7, 4, 4)
)

# Merge to include reference line per facet
plot_data <- plot_data %>%
  left_join(reference_lines, by = "Variable")

# Plot all in a single faceted plot
Baseline_MAE_RV <- ggplot(plot_data, aes(x = Parameter, y = RMSE)) +
  geom_line(color = "steelblue") +
  geom_vline(aes(xintercept = Reference), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Minimum MAE RV vs Each Parameter",
    x = "Parameter Value",
    y = "Minimum MAE RV"
  ) +
  theme_minimal()


###### MAE HV ###########
# Run for all Names
Baseline_results_list_MAE_HV <- lapply(
  unique(results$Name),
  get_min_metric_by_name,
  df = results,
  metric = MAE_HV
)

# Combine min values
Baseline_min_df_MAE_HV <- do.call(rbind, lapply(Baseline_results_list_MAE_HV, function(x) x$min_vector[1:4]))
rownames(Baseline_min_df_MAE_HV) <- unique(results$Name)

Baseline_min_final_MAE_HV <- as.data.frame(Baseline_min_df_MAE_HV) %>%
  mutate(
    final = pmin(a, b, kappa, gamma, na.rm = TRUE),
    Final_par = sapply(Baseline_results_list_MAE_HV, function(x) x$min_vector["Final_par"])
  )

# Extract summary tables
Baseline_summary_a_MAE_HV     <- lapply(Baseline_results_list_MAE_HV, function(x) x$summary_a)
Baseline_summary_b_MAE_HV     <- lapply(Baseline_results_list_MAE_HV, function(x) x$summary_b)
Baseline_summary_kappa_MAE_HV <- lapply(Baseline_results_list_MAE_HV, function(x) x$summary_kappa)
Baseline_summary_gamma_MAE_HV <- lapply(Baseline_results_list_MAE_HV, function(x) x$summary_gamma)

# Optional view
# View(Baseline_summary_a_MAE_HV[[1]])
# View(Baseline_summary_b_MAE_HV[[1]])
# View(Baseline_summary_kappa_MAE_HV[[1]])
# View(Baseline_summary_gamma_MAE_HV[[1]])

# Prepare the summary tables
param_summaries <- list(
  a     = Baseline_summary_a_MAE_HV[[1]] %>% select(Parameter = a, RMSE = Min_a),
  b     = Baseline_summary_b_MAE_HV[[1]] %>% select(Parameter = b, RMSE = Min_b),
  kappa = Baseline_summary_kappa_MAE_HV[[1]] %>% select(Parameter = kappa, RMSE = Min_kappa),
  gamma = Baseline_summary_gamma_MAE_HV[[1]] %>% select(Parameter = gamma, RMSE = Min_gamma)
)

# Combine into one data frame
plot_data <- bind_rows(
  lapply(names(param_summaries), function(param) {
    param_summaries[[param]] %>%
      mutate(Variable = param)
  }),
  .id = "ID"
)

# Define reference lines for each variable
reference_lines <- tibble(
  Variable = c("a", "b", "kappa", "gamma"),
  Reference = c(0.7, 0.7, 4, 4)
)

# Merge to include reference line per facet
plot_data <- plot_data %>%
  left_join(reference_lines, by = "Variable")

# Plot all in a single faceted plot
Baseline_MAE_HV <- ggplot(plot_data, aes(x = Parameter, y = RMSE)) +
  geom_line(color = "steelblue") +
  geom_vline(aes(xintercept = Reference), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Minimum MAE HV vs Each Parameter",
    x = "Parameter Value",
    y = "Minimum MAE HV"
  ) +
  theme_minimal()


###### AIC ###########
# Run for all Names
Baseline_results_list_AIC <- lapply(
  unique(results$Name),
  get_min_metric_by_name,
  df = results,
  metric = AIC
)

# Combine min values
Baseline_min_df_AIC <- do.call(rbind, lapply(Baseline_results_list_AIC, function(x) x$min_vector[1:4]))
rownames(Baseline_min_df_AIC) <- unique(results$Name)

Baseline_min_final_AIC <- as.data.frame(Baseline_min_df_AIC) %>%
  mutate(
    final = pmin(a, b, kappa, gamma, na.rm = TRUE),
    Final_par = sapply(Baseline_results_list_AIC, function(x) x$min_vector["Final_par"])
  )

# Extract summary tables
Baseline_summary_a_AIC     <- lapply(Baseline_results_list_AIC, function(x) x$summary_a)
Baseline_summary_b_AIC     <- lapply(Baseline_results_list_AIC, function(x) x$summary_b)
Baseline_summary_kappa_AIC <- lapply(Baseline_results_list_AIC, function(x) x$summary_kappa)
Baseline_summary_gamma_AIC <- lapply(Baseline_results_list_AIC, function(x) x$summary_gamma)

# Optional view
# View(Baseline_summary_a_AIC[[1]])
# View(Baseline_summary_b_AIC[[1]])
# View(Baseline_summary_kappa_AIC[[1]])
# View(Baseline_summary_gamma_AIC[[1]])

# Prepare the summary tables
param_summaries <- list(
  a     = Baseline_summary_a_AIC[[1]] %>% select(Parameter = a, RMSE = Min_a),
  b     = Baseline_summary_b_AIC[[1]] %>% select(Parameter = b, RMSE = Min_b),
  kappa = Baseline_summary_kappa_AIC[[1]] %>% select(Parameter = kappa, RMSE = Min_kappa),
  gamma = Baseline_summary_gamma_AIC[[1]] %>% select(Parameter = gamma, RMSE = Min_gamma)
)

# Combine into one data frame
plot_data <- bind_rows(
  lapply(names(param_summaries), function(param) {
    param_summaries[[param]] %>%
      mutate(Variable = param)
  }),
  .id = "ID"
)

# Define reference lines for each variable
reference_lines <- tibble(
  Variable = c("a", "b", "kappa", "gamma"),
  Reference = c(0.7, 0.7, 4, 4)
)

# Merge to include reference line per facet
plot_data <- plot_data %>%
  left_join(reference_lines, by = "Variable")

# Plot all in a single faceted plot
Baseline_AIC <- ggplot(plot_data, aes(x = Parameter, y = RMSE)) +
  geom_line(color = "steelblue") +
  geom_vline(aes(xintercept = Reference), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Minimum AIC vs Each Parameter",
    x = "Parameter Value",
    y = "Minimum AIC"
  ) +
  theme_minimal()

###### BIC ###########
# Run for all Names
Baseline_results_list_AIC <- lapply(
  unique(results$Name),
  get_min_metric_by_name,
  df = results,
  metric = AIC
)

# Combine min values
Baseline_min_df_AIC <- do.call(rbind, lapply(Baseline_results_list_AIC, function(x) x$min_vector[1:4]))
rownames(Baseline_min_df_AIC) <- unique(results$Name)

Baseline_min_final_AIC <- as.data.frame(Baseline_min_df_AIC) %>%
  mutate(
    final = pmin(a, b, kappa, gamma, na.rm = TRUE),
    Final_par = sapply(Baseline_results_list_AIC, function(x) x$min_vector["Final_par"])
  )

# Extract summary tables
Baseline_summary_a_AIC     <- lapply(Baseline_results_list_AIC, function(x) x$summary_a)
Baseline_summary_b_AIC     <- lapply(Baseline_results_list_AIC, function(x) x$summary_b)
Baseline_summary_kappa_AIC <- lapply(Baseline_results_list_AIC, function(x) x$summary_kappa)
Baseline_summary_gamma_AIC <- lapply(Baseline_results_list_AIC, function(x) x$summary_gamma)

# Optional view
# View(Baseline_summary_a_AIC[[1]])
# View(Baseline_summary_b_AIC[[1]])
# View(Baseline_summary_kappa_AIC[[1]])
# View(Baseline_summary_gamma_AIC[[1]])

# Prepare the summary tables
param_summaries <- list(
  a     = Baseline_summary_a_AIC[[1]] %>% select(Parameter = a, RMSE = Min_a),
  b     = Baseline_summary_b_AIC[[1]] %>% select(Parameter = b, RMSE = Min_b),
  kappa = Baseline_summary_kappa_AIC[[1]] %>% select(Parameter = kappa, RMSE = Min_kappa),
  gamma = Baseline_summary_gamma_AIC[[1]] %>% select(Parameter = gamma, RMSE = Min_gamma)
)

# Combine into one data frame
plot_data <- bind_rows(
  lapply(names(param_summaries), function(param) {
    param_summaries[[param]] %>%
      mutate(Variable = param)
  }),
  .id = "ID"
)

# Define reference lines for each variable
reference_lines <- tibble(
  Variable = c("a", "b", "kappa", "gamma"),
  Reference = c(0.7, 0.7, 4, 4)
)

# Merge to include reference line per facet
plot_data <- plot_data %>%
  left_join(reference_lines, by = "Variable")

# Plot all in a single faceted plot
Baseline_AIC <- ggplot(plot_data, aes(x = Parameter, y = RMSE)) +
  geom_line(color = "steelblue") +
  geom_vline(aes(xintercept = Reference), linetype = "dotted", color = "blue", size = 1) +
  facet_wrap(~ Variable, scales = "free") +
  labs(
    title = "Minimum AIC vs Each Parameter",
    x = "Parameter Value",
    y = "Minimum AIC"
  ) +
  theme_minimal()

###### Final graphs ###########

Baseline_RMSE_RV
Baseline_RMSE_HV
Baseline_MAE_RV
Baseline_MAE_HV
Baseline_AIC
Baseline_BIC


