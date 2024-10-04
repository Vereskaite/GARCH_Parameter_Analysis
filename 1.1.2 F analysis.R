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
                            a + 0.5 * b * ((exp(8 * P_t_in) - 1) / (exp(8 * P_t_in) + 1) - 
                                             (exp(4 * N_t_in) - 1) / (exp(4 * N_t_in) + 1)))
  
  grid$f_scenario_3 <- with(grid, 
                            a + 0.5 * b * ((exp(4 * P_t_in) - 1) / (exp(4 * P_t_in) + 1) - 
                                             (exp(8 * N_t_in) - 1) / (exp(8 * N_t_in) + 1)))
  
  grid$f_scenario_4 <- with(grid, 
                            a + 0.5 * b * ((exp(16 * P_t_in) - 1) / (exp(16 * P_t_in) + 1) - 
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



combined_plot <- plot_ly() %>%
  add_surface(x = P_values, y = N_values, z = z_values_scenario_1, name = "Scenario 1", 
              colorscale = list(c(0, "blue"), c(1, "lightblue")), showscale = TRUE, opacity = 0.7) %>%
  add_surface(x = P_values, y = N_values, z = z_values_scenario_2, name = "Scenario 2", 
              colorscale = list(c(0, "green"), c(1, "lightgreen")), showscale = TRUE, opacity = 0.7) %>%
  add_surface(x = P_values, y = N_values, z = z_values_scenario_3, name = "Scenario 3", 
              colorscale = list(c(0, "red"), c(1, "orange")), showscale = TRUE, opacity = 0.7) %>%
  layout(title = "Combined 3D Surface Plot for All Scenarios",
         scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "N"),
                      zaxis = list(title = "f_scenario")))


### with intersection
tolerance <- 0.04  # Define a tolerance for considering values equal
intersection_points <- (abs(z_values_scenario_1 - z_values_scenario_2) < tolerance) & 
  (abs(z_values_scenario_2 - z_values_scenario_3) < tolerance)

P_intersect <- rep(P_values, each = length(N_values))[intersection_points]
N_intersect <- rep(N_values, length(P_values))[intersection_points]
f_intersect <- z_values_scenario_1[intersection_points]

combined_plot_with_intersections <- plot_ly() %>%
  # add_surface(x = P_values, y = N_values, z = z_values_scenario_1, name = "Scenario 1", 
  #             colorscale = list(c(0, "blue"), c(1, "lightblue")), showscale = TRUE, opacity = 0.7) %>%
  add_surface(x = P_values, y = N_values, z = z_values_scenario_2, name = "Scenario 2", 
              colorscale = list(c(0, "green"), c(1, "lightgreen")), showscale = TRUE, opacity = 0.7) %>%
  add_surface(x = P_values, y = N_values, z = z_values_scenario_3, name = "Scenario 3", 
              colorscale = list(c(0, "red"), c(1, "orange")), showscale = TRUE, opacity = 0.7) %>%
  # Add intersection points as lines
  add_markers(x = P_intersect, y = N_intersect, z = f_intersect, 
              marker = list(color = 'black', size = 3), name = "Intersection Points") %>%
  layout(title = "Combined 3D Surface Plot with Intersections",
         scene = list(xaxis = list(title = "P"),
                      yaxis = list(title = "N"),
                      zaxis = list(title = "f_scenario")))

combined_plot_with_intersections


#### matrix

comparison_matrix <- matrix(NA, nrow = length(N_values), ncol = length(P_values))

for (i in 1:length(N_values)) {
  for (j in 1:length(P_values)) {
    if (z_values_scenario_2[i, j] > z_values_scenario_3[i, j]) {
      comparison_matrix[i, j] <- "Scenario 2"
    } else if (z_values_scenario_2[i, j] < z_values_scenario_3[i, j]) {
      comparison_matrix[i, j] <- "Scenario 3"
    } else {
      comparison_matrix[i, j] <- "Equal"
    }
  }
}
# comparison_matrix %>% View()

comparison_df <- as.data.frame(comparison_matrix, row.names = N_values, col.names = P_values)



# # Convert to data frame for plotting
# comparison_df <- as.data.frame(comparison_numeric)
# colnames(comparison_df) <- P_values
# rownames(comparison_df) <- N_values
# 
# # Melt the data frame for ggplot2 compatibility
# comparison_melted <- melt(comparison_df, varnames = c("N", "P"), value.name = "Scenario")
# 
# # Plot the heatmap
# heatmap_plot <- ggplot(comparison_melted, aes(x = P, y = N, fill = Scenario)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
#                        midpoint = 0, 
#                        name = "Scenario") +
#   labs(title = "Heatmap of Scenario Dominance", 
#        x = "P", 
#        y = "N") +
#   theme_minimal()
# 
# # Display the heatmap
# heatmap_plot

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


### Auxiliary #####
a <- 0.8
b <- 0.8
kappa <- 4
gamma <- 8
P_t_in <- 1
N_t_in <- -0.1

a + 0.5 * b * ((exp(kappa * P_t_in) - 1) / (exp(kappa * P_t_in) + 1) - (exp(gamma * N_t_in) - 1) / (exp(gamma * N_t_in) + 1))

head(PN_dt)
### all else equal P observation
PN_dt %>% 
  filter(N == "-0.1") %>% 
  # mutate(Index = 1:10) %>% 
  ggplot()+
  geom_line(aes(x=P, y=f_scenario_1), color = "black")+
  geom_line(aes(x=P, y=f_scenario_2), color = "red")+
  geom_line(aes(x=P, y=f_scenario_3), color = "blue")+
geom_line(aes(x=P, y=f_scenario_4), color = "brown")
