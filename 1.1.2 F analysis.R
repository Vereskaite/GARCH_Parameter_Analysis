library(patchwork)
library(gridExtra)

###### general f analysis for function understanding ############
### creating 3 scenarios
## Scenario 1: kappa = gamma = 4
## Scenario 2: kappa = 8, gamma = 4
## Scenario 3: kappa = 4, gamma = 8

generate_parameter_grid_PN <- function(P_range, N_range, P_step, N_step, a, b) { 
  P_values <- seq(P_range[1], P_range[2], by = P_step)
  N_values <- seq(N_range[1], N_range[2], by = N_step)
  
  grid <- expand.grid(P_t_in = P_values, N_t_in = N_values)
  
  # Calculate f for each scenario
  grid$f_scenario_1 <- with(grid, 
                            a + 0.5 * b * ((exp(4 * P_t_in) - 1) / (exp(4 * P_t_in) + 1) - 
                                             (exp(4 * N_t_in) - 1) / (exp(4 * N_t_in) + 1)))
  
  grid$f_scenario_2 <- with(grid, 
                            a + 0.5 * b * ((exp(4 * P_t_in) - 1) / (exp(4 * P_t_in) + 1) - 
                                             (exp(8 * N_t_in) - 1) / (exp(8 * N_t_in) + 1)))
  
  grid$f_scenario_3 <- with(grid, 
                            a + 0.5 * b * ((exp(8 * P_t_in) - 1) / (exp(8 * P_t_in) + 1) - 
                                             (exp(4 * N_t_in) - 1) / (exp(4 * N_t_in) + 1)))
  
  return(grid)
}

parameter_grid_PN <- generate_parameter_grid_PN(P_range = c(0, 1), 
                                          P_step = 0.1,
                                          N_range = c(-1, 0), 
                                          N_step = 0.1,
                                          a = 0.8, b = 0.8)

PN_dt <- parameter_grid_PN %>% 
  mutate(Positive_higher = f_scenario_2 - f_scenario_1,
         Positive_lower = f_scenario_3 - f_scenario_1) %>% 
  rename(P = P_t_in,
         N = N_t_in) %>% 
  filter(P > 0 & N <0)



### 3D surfaces #######

P_values <- unique(PN_dt$P)
N_values <- unique(PN_dt$N)

# Reshape f_scenario_1, f_scenario_2, and f_scenario_3 into matrices
z_values_scenario_1 <- matrix(PN_dt$f_scenario_1, nrow = length(N_values), ncol = length(P_values), byrow = TRUE)
z_values_scenario_2 <- matrix(PN_dt$f_scenario_2, nrow = length(N_values), ncol = length(P_values), byrow = TRUE)
z_values_scenario_3 <- matrix(PN_dt$f_scenario_3, nrow = length(N_values), ncol = length(P_values), byrow = TRUE)

# 3D Surface Plot for f_scenario_1
plot_scenario_1 <- plot_ly(x = P_values, y = N_values, z = z_values_scenario_1, 
                           type = "surface") %>%
  layout(title = "3D Surface for f_scenario_1",
         scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "N"),
                      zaxis = list(title = "f_scenario_1")))

# 3D Surface Plot for f_scenario_2
plot_scenario_2 <- plot_ly(x = P_values, y = N_values, z = z_values_scenario_2, 
                           type = "surface") %>%
  layout(title = "3D Surface for f_scenario_2",
         scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "N"),
                      zaxis = list(title = "f_scenario_2")))

# 3D Surface Plot for f_scenario_3
plot_scenario_3 <- plot_ly(x = P_values, y = N_values, z = z_values_scenario_3, 
                           type = "surface") %>%
  layout(title = "3D Surface for f_scenario_3",
         scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "N"),
                      zaxis = list(title = "f_scenario_3")))

plot_scenario_1
plot_scenario_2
plot_scenario_3

##### Heatmpas of f difference #######

heatmap_pos_higher <- ggplot(PN_dt, aes(x = P, y = N, fill = Positive_higher)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Higher Positive Impact\n(kappa > gamma)", x = "P", y = "N") +
  theme_minimal() +
  scale_y_reverse() +
  theme(legend.position = "none")  # Hide legend for this plot

heatmap_pos_lower <- ggplot(PN_dt, aes(x = P, y = N, fill = Positive_lower)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Higher Negative Impact\n(kappa < gamma)", x = "P", y = "N") +
  theme_minimal() +
  scale_y_reverse() +
  theme(legend.position = "none")  # Hide legend for this plot

combined_heatmap <- heatmap_pos_higher + 
  heatmap_pos_lower + 
  plot_layout(ncol = 2) +  # Arrange heatmaps in one row
  plot_annotation(title = "Heatmaps of function value difference compared to baseline (kappa = gamma)") +
  plot_layout(guides = "collect")  # Collect legends

Heatmap_f_difference <- combined_heatmap + plot_layout(ncol = 2, guides = "collect")


### f difference lines ########

f_diff_PN_lines <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 1) +
  geom_line(data = PN_dt, aes(x = N, y = Positive_higher, color = "Pos_higher"), size = 1) +
  geom_line(data = PN_dt, aes(x = P, y = Positive_lower, color = "Pos_lower"), size = 1) +
  scale_color_manual(values = c("f_diff" = "blue", "Pos_higher" = "red", "Pos_lower" = "green"),
                     name = "Legend") +
  labs(title = "Graph of f_diff with Pos_higher and Pos_lower",
       x = "P (for Pos_lower) and N (for Pos_higher)",
       y = "Values") +
  theme_minimal() +
  annotate("text", x = max(PN_dt$P_t_in), y = 0.05, label = "Baseline (kappa = gamma)", color = "black", hjust = 1)



### Comparing 3 scenarios f values ######

PN_long <- melt(PN_dt, id.vars = c("P", "N"), 
                measure.vars = c("f_scenario_1", "f_scenario_2", "f_scenario_3"),
                variable.name = "Scenario", value.name = "f_value")

plot_scenarios_smoothed_P <- ggplot(PN_long, aes(x = P, y = f_value, color = Scenario)) +
  geom_line(size = 1, alpha = 0.2) +  # Original lines with low transparency
  geom_smooth(method = "loess", se = FALSE, linetype = "solid", size = 1) +  # Smoothed curves
  labs(
    # title = "Comparison of f Values Across Three Scenarios (With Smoothed Curves)",
       x = "P value",
       y = "f Value") +
  scale_color_manual(values = c("f_scenario_1" = "blue", 
                                "f_scenario_2" = "red", 
                                "f_scenario_3" = "green"),
                     name = "Scenarios") +
  theme_minimal() +
  theme(legend.position = "none")


plot_scenarios_smoothed_N <- ggplot(PN_long, aes(x = N, y = f_value, color = Scenario)) +
  geom_line(size = 1, alpha = 0.2) +  # Original lines with low transparency
  geom_smooth(method = "loess", se = FALSE, linetype = "solid", size = 1) +  # Smoothed curves
  labs(
    # title = "Comparison of f Values Across Three Scenarios (With Smoothed Curves)",
       x = "N value",
       y = "f Value") +
  scale_color_manual(values = c("f_scenario_1" = "blue", 
                                "f_scenario_2" = "red", 
                                "f_scenario_3" = "green"),
                     name = "Scenarios") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(
    axis.title.y = element_blank(),  # Remove y-axis title
    axis.text.y = element_blank(),    # Remove y-axis tick labels
    axis.ticks.y = element_blank()    # Optionally remove y-axis ticks
  )

plot_scenarios_smoothed_PN <-  plot_scenarios_smoothed_N + plot_scenarios_smoothed_P +
  plot_annotation(title = "Comparison of f Values Across Different Scenarios")


###### selected f analysis distributions ###########
hist(Data_k4_g4$Positive_Sentiment)
hist(Data_k4_g4$Negative_Sentiment)
hist(Data_k4_g4$f_t)
