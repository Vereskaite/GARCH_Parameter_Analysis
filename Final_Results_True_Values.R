# Data
results_true

####### Per metric - FINAL FINAL FINAL

### Define main function
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
      pivot_wider(names_from = Combination, values_from = Metric)
  }
  
  # Define label formats for each case
  combo_a <- function(a, b, kappa, gamma) paste0("b", b, "_k", kappa, "_g", gamma)
  combo_b <- function(a, b, kappa, gamma) paste0("a", a, "_k", kappa, "_g", gamma)
  combo_kappa <- function(a, b, kappa, gamma) paste0("a", a, "_b", b, "_g", gamma)
  combo_gamma <- function(a, b, kappa, gamma) paste0("a", a, "_b", b, "_k", kappa)
  
  # Generate summaries
  summary_a <- summarize_by_params(df_name, a, combo_a)
  summary_b <- summarize_by_params(df_name, b, combo_b)
  summary_kappa <- summarize_by_params(df_name, kappa, combo_kappa)
  summary_gamma <- summarize_by_params(df_name, gamma, combo_gamma)
  
  # Add Min and Final_par columns
  add_min_cols <- function(df, group_var, suffix) {
    df %>%
      mutate(
        !!paste0("Min_", suffix) := apply(across(-{{ group_var }}), 1, min, na.rm = TRUE),
        Final_par = apply(across(-{{ group_var }}), 1, function(x) names(x)[which.min(x)])
      )
  }
  
  summary_a <- add_min_cols(summary_a, a, "a")
  summary_b <- add_min_cols(summary_b, b, "b")
  summary_kappa <- add_min_cols(summary_kappa, kappa, "kappa")
  summary_gamma <- add_min_cols(summary_gamma, gamma, "gamma")
  
  # Extract min metric values
  min_vals <- c(
    a = min(summary_a[[paste0("Min_", "a")]], na.rm = TRUE),
    b = min(summary_b[[paste0("Min_", "b")]], na.rm = TRUE),
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

#### Run primary tables #########
####### RMSE_RV #########
results_list_RMSE_RV <- lapply(
  unique(results_true$Name),
  get_min_metric_by_name,
  df     = results_true,
  metric = RMSE_RV
)

min_df_RMSE_RV <- do.call(rbind, lapply(results_list_RMSE_RV, function(x) x$min_vector[1:4]))
rownames(min_df_RMSE_RV) <- unique(results_true$Name)

min_final_RMSE_RV <- as.data.frame(min_df_RMSE_RV) %>%
  mutate(final = pmin(a, b, kappa, gamma, na.rm = TRUE),
         Final_par = sapply(results_list_RMSE_RV, function(x) x$min_vector["Final_par"]))


summary_a_RMSE_RV <- lapply(results_list_RMSE_RV, function(x) x$summary_a)
summary_b_RMSE_RV <- lapply(results_list_RMSE_RV, function(x) x$summary_b)
summary_kappa_RMSE_RV <- lapply(results_list_RMSE_RV, function(x) x$summary_kappa)
summary_gamma_RMSE_RV <- lapply(results_list_RMSE_RV, function(x) x$summary_gamma)

# Optionally, view one of the summaries (e.g., for the first Name)
View(summary_a_RMSE_RV[[1]])
View(summary_b_RMSE_RV[[1]])
View(summary_kappa_RMSE_RV[[1]])
View(summary_gamma_RMSE_RV[[1]])



####### RMSE_HV #########
results_list_RMSE_HV <- lapply(
  unique(results_true$Name),
  get_min_metric_by_name,
  df     = results_true,
  metric = RMSE_HV
)

min_df_RMSE_HV <- do.call(rbind, lapply(results_list_RMSE_HV, function(x) x$min_vector[1:4]))
rownames(min_df_RMSE_HV) <- unique(results_true$Name)

min_final_RMSE_HV <- as.data.frame(min_df_RMSE_HV) %>%
  mutate(final = pmin(a, b, kappa, gamma, na.rm = TRUE),
         Final_par = sapply(results_list_RMSE_HV, function(x) x$min_vector["Final_par"]))


summary_a_RMSE_HV <- lapply(results_list_RMSE_HV, function(x) x$summary_a)
summary_b_RMSE_HV <- lapply(results_list_RMSE_HV, function(x) x$rmse_summary_b)
summary_kappa_RMSE_HV <- lapply(results_list_RMSE_HV, function(x) x$summary_kappa)
summary_gamma_RMSE_HV <- lapply(results_list_RMSE_HV, function(x) x$summary_gamma)

# Optionally, view one of the summaries (e.g., for the first Name)
View(summary_a_RMSE_HV[[1]])
View(summary_b_RMSE_HV[[1]])
View(summary_kappa_RMSE_HV[[1]])
View(summary_gamma_RMSE_HV[[1]])

####### AIC #########
results_list_AIC <- lapply(
  unique(results_true$Name),
  get_min_metric_by_name,
  df     = results_true,
  metric = AIC
)

min_df_AIC <- do.call(rbind, lapply(results_list_AIC, function(x) x$min_vector[1:4]))
rownames(min_df_AIC) <- unique(results_true$Name)

min_final_AIC <- as.data.frame(min_df_AIC) %>%
  mutate(final = pmin(a, b, kappa, gamma, na.rm = TRUE),
         Final_par = sapply(results_list_AIC, function(x) x$min_vector["Final_par"]))


summary_a_AIC <- lapply(results_list_AIC, function(x) x$summary_a)
summary_b_AIC <- lapply(results_list_AIC, function(x) x$rmse_summary_b)
summary_kappa_AIC <- lapply(results_list_AIC, function(x) x$summary_kappa)
summary_gamma_AIC <- lapply(results_list_AIC, function(x) x$summary_gamma)

# Optionally, view one of the summaries (e.g., for the first Name)
View(summary_a_AIC[[1]])
View(summary_b_AIC[[1]])
View(summary_kappa_AIC[[1]])
View(summary_gamma_AIC[[1]])

#### Additional data tables ########
# Per metric
min_final_RMSE_RV
min_final_RMSE_HV
min_final_AIC

# Per metric and per name, shows all combinations, and final one
View(summary_a_RMSE_HV[[1]]) # 1 means baseline

# Final table
Final_true_parameters_table <- min_final_RMSE_RV %>% 
  select(final) %>% 
rename(RMSE_RV = final) %>% 
  bind_cols(min_final_RMSE_HV %>% 
              select(final) %>% 
              rename(RMSE_HV = final)) %>% 
  bind_cols(min_final_AIC%>% 
              select(final) %>% 
              rename(AIC = final)) 


##### a analysis ########

a_summary_Aux <- do.call(rbind, lapply(seq_along(summary_a_RMSE_RV), function(i) {
  df <- summary_a_RMSE_RV[[i]]
  name <- unique(results_true$Name)[i]
  
  df_selected <- df[, c("a", "Min_a", "Final_par")]
  df_selected$Name <- name
  return(df_selected)
}))

a_summary <- a_summary_Aux %>%
  group_by(Name) %>%
  slice_min(order_by = Min_a, with_ties = FALSE) %>%
  ungroup() %>%
  select(Name, a, Min_a, Final_par)

##### b analysis
b_summary_Aux <- do.call(rbind, lapply(seq_along(summary_b_RMSE_RV), function(i) {
  df <- summary_b_RMSE_RV[[i]]
  name <- unique(results_true$Name)[i]
  
  df_selected <- df[, c("b", "Min_b", "Final_par")]
  df_selected$Name <- name
  return(df_selected)
}))


b_summary <- b_summary_Aux %>%
  group_by(Name) %>%
  slice_min(order_by = Min_b, with_ties = FALSE) %>%
  ungroup() %>%
  select(Name, b, Min_b, Final_par)



library(purrr)

# Helper function to extract minimal row for each parameter
get_param_summary <- function(summary_list, param_name, min_col_name) {
  summary_aux <- do.call(rbind, lapply(seq_along(summary_list), function(i) {
    df <- summary_list[[i]]
    name <- unique(results_true$Name)[i]
    
    df_selected <- df[, c(param_name, min_col_name, "Final_par")]
    df_selected$Name <- name
    return(df_selected)
  }))
  
  summary_final <- summary_aux %>%
    group_by(Name) %>%
    slice_min(order_by = .data[[min_col_name]], with_ties = FALSE) %>%
    ungroup() %>%
    select(Name, !!param_name, !!min_col_name, Final_par)
  
  # Rename columns to indicate parameter
  colnames(summary_final)[2:4] <- paste0(param_name, "_", c("value", "min", "Final_par"))
  return(summary_final)
}

# Apply to all parameters
a_summary <- get_param_summary(summary_a_RMSE_RV, "a", "Min_a")
b_summary <- get_param_summary(summary_b_RMSE_RV, "b", "Min_b")
kappa_summary <- get_param_summary(summary_kappa_RMSE_RV, "kappa", "Min_kappa")
gamma_summary <- get_param_summary(summary_gamma_RMSE_RV, "gamma", "Min_gamma")

# Join all by Name
final_summary_Aux <- reduce(list(a_summary, b_summary, kappa_summary, gamma_summary), left_join, by = "Name")

Final_par_table_RMSE_RV <- final_summary_Aux %>%  
  mutate(a_Final_par = paste0("a", a_value, "_", a_Final_par),
         gamma_Final_par = paste0(gamma_Final_par, "_g", gamma_value),
         b_Final_par = paste0("a", gsub("^a(.*?)_.*$", "\\1", b_Final_par), "_b", b_value, "_k", gsub(".*k(.*)$", "\\1", b_Final_par)),
         kappa_Final_par = paste0(gsub("_g.*", "_", kappa_Final_par),"k",kappa_value,"_g",sub(".*_g", "", kappa_Final_par) ))


# Create the table for a/RMSE analysis

## a
a_RMSE_min_table <- do.call(rbind, lapply(seq_along(summary_a_RMSE_RV), function(i) {
  df <- summary_a_RMSE_RV[[i]]
  name <- unique(results_true$Name)[i]
  
  df %>%
    group_by(a) %>%
    summarise(RMSE_min = min(Min_a, na.rm = TRUE), .groups = "drop") %>%
    mutate(Name = name)
})) %>%
  select(Name, a, RMSE_min)

ggplot(a_RMSE_min_table, aes(x = a, y = RMSE_min)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkred") +
  facet_wrap(~ Name, scales = "free_y") +
  theme_minimal() +
  labs(title = "RMSE_min vs. a for each Name",
       x = "a",
       y = "Minimum RMSE")

# Gamma
gamma_RMSE_min_table <- do.call(rbind, lapply(seq_along(summary_gamma_RMSE_RV), function(i) {
  df <- summary_gamma_RMSE_RV[[i]]
  name <- unique(results_true$Name)[i]
  
  df %>%
    group_by(gamma) %>%
    summarise(RMSE_min = min(Min_gamma, na.rm = TRUE), .groups = "drop") %>%
    mutate(Name = name)
})) %>%
  select(Name, gamma, RMSE_min)

library(ggplot2)
library(dplyr)

# Create a named vector of actual gamma values for each Name
gamma_values <- c("Baseline" = 4, "Gamma=0.5" = 0.5, "Gamma=2" = 2, "Gamma=10" = 10, "Gamma=20" = 20)

# Convert it into a data frame for use with geom_vline
vline_data <- data.frame(
  Name = names(gamma_values),
  gamma_vline = unname(gamma_values)
)

# Plot
Gamma_true_par_graph <- ggplot(gamma_RMSE_min_table %>% 
         filter(Name %in% names(gamma_values)), 
       aes(x = gamma, y = RMSE_min)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkred") +
  geom_vline(data = vline_data, aes(xintercept = gamma_vline), 
             color = "black", linetype = "dashed") +
  facet_wrap(~ Name, scales = "free_y") +
  theme_minimal() +
  labs(title = "RMSE_min vs. gamma for each Name",
       x = "gamma",
       y = "Minimum RMSE")


# Kappa
kappa_RMSE_min_table <- do.call(rbind, lapply(seq_along(summary_kappa_RMSE_RV), function(i) {
  df <- summary_kappa_RMSE_RV[[i]]
  name <- unique(results_true$Name)[i]
  
  df %>%
    group_by(kappa) %>%
    summarise(RMSE_min = min(Min_kappa, na.rm = TRUE), .groups = "drop") %>%
    mutate(Name = name)
})) %>%
  select(Name, kappa, RMSE_min)

kappa_values <- c("Baseline" = 4, "Kappa=0.5" = 0.5, "Kappa=2" = 2, "Kappa=10" = 10, "Kappa=20" = 20)

# Create a data frame to use in geom_vline
vline_data_kappa <- data.frame(
  Name = names(kappa_values),
  kappa_vline = unname(kappa_values)
)

# Plot
Kappa_true_par_graph <- ggplot(kappa_RMSE_min_table %>% 
         filter(Name %in% names(kappa_values)), 
       aes(x = kappa, y = RMSE_min)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkred") +
  geom_vline(data = vline_data_kappa, aes(xintercept = kappa_vline), 
             color = "black", linetype = "dashed") +
  facet_wrap(~ Name, scales = "free_y") +
  theme_minimal() +
  labs(title = "RMSE_min vs. kappa for each Name",
       x = "kappa",
       y = "Minimum RMSE")



##### FINAL tables and graph ##########
### Tables
# Ultimate final table
Final_true_parameters_table

# Parameters and combinations table for each Name (RMSE RV)
Final_par_table_RMSE_RV

# parameter and it's combination RMSE RV valuexs for Baseline (granular data)
summary_a_RMSE_RV[[1]] %>% View()
summary_b_RMSE_RV[[1]]
summary_kappa_RMSE_RV[[1]]
summary_gamma_RMSE_RV[[1]]

### Graphs
Gamma_true_par_graph
Kappa_true_par_graph
