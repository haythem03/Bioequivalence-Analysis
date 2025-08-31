library(rxode2)
library(dplyr)
library(ggplot2)
library(tidyr)
library(pracma)

# Initial model setup
ka <- 0.7
cl <- 6
vd <- 35

# Enhanced model with bioavailability (F) and lag time (Tlag)
model <- rxode2({
  ka <- ka
  cl <- cl
  v  <- vd
  f  <- f        # Bioavailability
  tlag <- tlag   # Lag time
  
  d/dt(depot)   = -ka * depot
  d/dt(central) =  ka * depot - (cl/v) * central
  
  cp = central / v
})

# Define an oral dose of 100 mg given at time 0
event <- eventTable()
event$add.dosing(dose = 100, nbr.doses = 1, dosing.to = 1, start.time = 0)
# Define time grid: simulate over 24 hours
event$add.sampling(seq(0, 24, by = 0.1))

# Define the parameters as a named list (including F and Tlag)
params <- c(ka = 0.7, cl = 6, vd = 35, f = 0.6, tlag = 0.0)

# Single subject simulation
result <- rxSolve(model, event, params = params)

cat("=== Single Subject Simulation Results ===\n")
print(head(result))

# Plot single subject
ggplot(result, aes(x = time, y = cp)) +
  geom_line(color = "darkgreen", size = 1.2) +
  labs(
    title = "Plasma Concentration-Time Curve (Single Subject)",
    x = "Time (h)",
    y = "Plasma Concentration (mg/L)"
  ) +
  theme_minimal()

# Population simulation parameters
n_subjects <- 1000
set.seed(42)

# Generate subject-specific parameters for REFERENCE formulation
pop_params_ref <- tibble(
  ka = rlnorm(n_subjects, log(0.7), 0.1),
  cl = rlnorm(n_subjects, log(6), 0.15),
  vd = rlnorm(n_subjects, log(35), 0.1),
  f = rlnorm(n_subjects, log(0.6), 0.05),    # Small variability in bioavailability
  tlag = rlnorm(n_subjects, log(0.1), 0.3)   # Small lag time with variability
)

cat("\n=== Reference Population Parameters (First 10 subjects) ===\n")
print(head(pop_params_ref, 10))

# Define dosing event and sampling times
event <- eventTable()
event$add.dosing(dose = 100, nbr.doses = 1, dosing.to = 1, start.time = 0)
event$add.sampling(seq(0, 24, by = 0.1))

# Run simulation for all subjects (Reference)
sim_results_ref <- rxSolve(model, params = pop_params_ref, events = event)

cat("\n=== Reference Simulation Results (First 10 rows) ===\n")
print(head(sim_results_ref, 10))

# Plot average concentration profile for reference
sim_results_ref %>%
  group_by(time) %>%
  summarize(mean_cp = mean(cp), .groups = "drop") %>%
  ggplot(aes(x = time, y = mean_cp)) +
  geom_line(color = "blue", linewidth = 1.2) +
  labs(
    title = "Mean Plasma Concentration-Time Curve - Reference (N=1000)",
    x = "Time (h)",
    y = "Plasma Concentration (mg/L)"
  ) +
  theme_minimal()

#==============================================================================
# MANUAL TEST PARAMETER INPUT SECTION
#==============================================================================

cat("\n=== MANUAL TEST PARAMETER CONFIGURATION ===\n")

# Define your exact test parameters here - enter the actual values you want
test_parameters <- list(
  ka_mean = 0.5,        # New mean absorption rate (original: 0.8)
  cl_mean = 6.0,         # New mean clearance (original: 6.0)
  vd_mean = 35.0,       # New mean volume of distribution (original: 35.0)
  f_mean = 0.65,         # New mean bioavailability (original: 1.0)
  tlag_mean = 2.0       # New mean lag time (original: 0.1)
)

# Calculate the factors for display purposes
ref_means <- list(
  ka_mean = 0.7,
  cl_mean = 6.0,
  vd_mean = 35.0,
  f_mean = 0.6,
  tlag_mean = 0.1
)

cat("Test parameters entered:\n")
cat("- ka mean:", test_parameters$ka_mean, "(vs ref:", ref_means$ka_mean, 
    ", change:", round((test_parameters$ka_mean/ref_means$ka_mean - 1)*100, 1), "%)\n")
cat("- cl mean:", test_parameters$cl_mean, "(vs ref:", ref_means$cl_mean, 
    ", change:", round((test_parameters$cl_mean/ref_means$cl_mean - 1)*100, 1), "%)\n")
cat("- vd mean:", test_parameters$vd_mean, "(vs ref:", ref_means$vd_mean, 
    ", change:", round((test_parameters$vd_mean/ref_means$vd_mean - 1)*100, 1), "%)\n")
cat("- f mean:", test_parameters$f_mean, "(vs ref:", ref_means$f_mean, 
    ", change:", round((test_parameters$f_mean/ref_means$f_mean - 1)*100, 1), "%)\n")
cat("- tlag mean:", test_parameters$tlag_mean, "(vs ref:", ref_means$tlag_mean, 
    ", change:", round((test_parameters$tlag_mean/ref_means$tlag_mean - 1)*100, 1), "%)\n")

# Generate test parameters with the new means but same variability structure
set.seed(42)  # Same seed to maintain subject pairing
pop_params_test <- tibble(
  ka = rlnorm(n_subjects, log(test_parameters$ka_mean), 0.1),
  cl = rlnorm(n_subjects, log(test_parameters$cl_mean), 0.15),
  vd = rlnorm(n_subjects, log(test_parameters$vd_mean), 0.1),
  f = rlnorm(n_subjects, log(test_parameters$f_mean), 0.05),
  tlag = rlnorm(n_subjects, log(test_parameters$tlag_mean), 0.3)
)

cat("\n=== Test Population Parameters (First 10 subjects) ===\n")
print(head(pop_params_test, 10))

# Parameter comparison summary
param_comparison <- tibble(
  Parameter = c("ka", "cl", "vd", "f", "tlag"),
  Reference_Mean = c(mean(pop_params_ref$ka), mean(pop_params_ref$cl), 
                     mean(pop_params_ref$vd), mean(pop_params_ref$f), 
                     mean(pop_params_ref$tlag)),
  Test_Mean = c(mean(pop_params_test$ka), mean(pop_params_test$cl), 
                mean(pop_params_test$vd), mean(pop_params_test$f), 
                mean(pop_params_test$tlag)),
  Test_Target = c(test_parameters$ka_mean, test_parameters$cl_mean,
                  test_parameters$vd_mean, test_parameters$f_mean,
                  test_parameters$tlag_mean),
  Ratio = Test_Mean / Reference_Mean,
  Percent_Change = (Ratio - 1) * 100
)

cat("\n=== Parameter Comparison Summary ===\n")
print(param_comparison)

#==============================================================================
# SIMULATION EXECUTION
#==============================================================================

# Simulate Reference
ref_sim <- rxSolve(model, params = pop_params_ref, events = event)
ref_sim <- ref_sim %>% mutate(Formulation = "Reference")

# Simulate Test
test_sim <- rxSolve(model, params = pop_params_test, events = event)
test_sim <- test_sim %>% mutate(Formulation = "Test")

# Combine results
combined_sim <- bind_rows(ref_sim, test_sim)

cat("\n=== Combined Simulation Results (First 10 rows) ===\n")
print(head(combined_sim, 10))

# Calculate summary statistics for visualization
summary_df <- combined_sim %>%
  group_by(Formulation, time) %>%
  summarize(
    mean_cp = mean(cp),
    lower = quantile(cp, 0.05),
    upper = quantile(cp, 0.95),
    .groups = "drop"
  )

cat("\n=== Summary Statistics (First 10 rows) ===\n")
print(head(summary_df, 10))

# Plot comparison of Reference vs Test
ggplot(summary_df, aes(x = time, y = mean_cp, color = Formulation)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = Formulation), alpha = 0.2) +
  labs(
    title = "Mean Plasma Concentration-Time Curves: Reference vs Test",
    subtitle = "Mean ± 90% Prediction Interval",
    x = "Time (h)",
    y = "Plasma Concentration (mg/L)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Reference" = "blue", "Test" = "red")) +
  scale_fill_manual(values = c("Reference" = "blue", "Test" = "red"))

# Calculate PK metrics
pk_metrics <- combined_sim %>%
  group_by(Formulation, sim.id) %>%
  summarize(
    Cmax = max(cp),
    Tmax = time[which.max(cp)],
    AUC = trapz(time, cp),
    .groups = "drop"
  )

cat("\n=== PK Metrics (First 10 rows) ===\n")
print(head(pk_metrics, 10))

# Pivot wider for bioequivalence analysis
pk_wide <- pk_metrics %>%
  pivot_wider(
    id_cols = sim.id,
    names_from = Formulation,
    values_from = c(Cmax, Tmax, AUC)
  )

cat("\n=== Wide Format PK Data (First 10 rows) ===\n")
print(head(pk_wide, 10))

#==============================================================================
# BIOEQUIVALENCE ANALYSIS
#==============================================================================

# Log-transform AUC and Cmax for bioequivalence testing
pk_wide <- pk_wide %>%
  mutate(
    log_Cmax_T = log(Cmax_Test),
    log_Cmax_R = log(Cmax_Reference),
    log_AUC_T  = log(AUC_Test),
    log_AUC_R  = log(AUC_Reference)
  )

cat("\n=== Log-transformed Data (First 10 rows) ===\n")
print(head(pk_wide %>% select(sim.id, log_Cmax_T, log_Cmax_R, log_AUC_T, log_AUC_R), 10))

# Perform paired t-test for log(AUC)
auc_result <- t.test(
  pk_wide$log_AUC_T,
  pk_wide$log_AUC_R,
  paired = TRUE,
  conf.level = 0.90
)

# Perform paired t-test for log(Cmax)
cmax_result <- t.test(
  pk_wide$log_Cmax_T,
  pk_wide$log_Cmax_R,
  paired = TRUE,
  conf.level = 0.90
)

# Print results
cat("\n🔬 BIOEQUIVALENCE ANALYSIS RESULTS:\n")
cat("=====================================\n")
cat("AUC (log scale):\n")
print(auc_result)
cat("\nCmax (log scale):\n")
print(cmax_result)

# Check GO/NO-GO Decision
lower_bound <- log(0.8)
upper_bound <- log(1.25)

auc_passes <- (auc_result$conf.int[1] > lower_bound) && (auc_result$conf.int[2] < upper_bound)
cmax_passes <- (cmax_result$conf.int[1] > lower_bound) && (cmax_result$conf.int[2] < upper_bound)

cat("\n🟢 DECISION SUMMARY:\n")
cat("===================\n")
cat("AUC passes bioequivalence?  ", ifelse(auc_passes, "✅ GO", "❌ NO-GO"), "\n")
cat("Cmax passes bioequivalence? ", ifelse(cmax_passes, "✅ GO", "❌ NO-GO"), "\n")
cat("Overall decision:           ", ifelse(auc_passes && cmax_passes, "✅ BIOEQUIVALENT", "❌ NOT BIOEQUIVALENT"), "\n")

#==============================================================================
# ENHANCED VISUALIZATIONS
#==============================================================================

# Calculate ratios for visualization
pk_wide <- pk_wide %>%
  mutate(
    AUC_ratio   = AUC_Test / AUC_Reference,
    Cmax_ratio  = Cmax_Test / Cmax_Reference,
    log_AUC_ratio = log(AUC_ratio),
    log_Cmax_ratio = log(Cmax_ratio)
  )

cat("\n=== Ratio Data (First 10 rows) ===\n")
print(head(pk_wide %>% select(sim.id, AUC_ratio, Cmax_ratio, log_AUC_ratio, log_Cmax_ratio), 10))

# AUC log-ratio density
p1 <- ggplot(pk_wide, aes(x = log_AUC_ratio)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = log(0.8), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = log(1.25), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = auc_result$conf.int[1], linetype = "dotted", color = "darkblue") +
  geom_vline(xintercept = auc_result$conf.int[2], linetype = "dotted", color = "darkblue") +
  labs(title = "Distribution of log(Test/Reference) AUC Ratios",
       subtitle = "Red dashed: BE limits, Blue dotted: 90% CI",
       x = "Log AUC Ratio", y = "Density") +
  theme_minimal()

# Cmax log-ratio density
p2 <- ggplot(pk_wide, aes(x = log_Cmax_ratio)) +
  geom_density(fill = "salmon", alpha = 0.6) +
  geom_vline(xintercept = log(0.8), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = log(1.25), linetype = "dashed", color = "red", size = 1) +
  geom_vline(xintercept = cmax_result$conf.int[1], linetype = "dotted", color = "darkblue") +
  geom_vline(xintercept = cmax_result$conf.int[2], linetype = "dotted", color = "darkblue") +
  labs(title = "Distribution of log(Test/Reference) Cmax Ratios",
       subtitle = "Red dashed: BE limits, Blue dotted: 90% CI",
       x = "Log Cmax Ratio", y = "Density") +
  theme_minimal()

print(p1)
print(p2)

# Reshape for boxplot
pk_long <- pk_wide %>%
  select(sim.id, AUC_Test, AUC_Reference, Cmax_Test, Cmax_Reference) %>%
  pivot_longer(cols = -sim.id,
               names_to = c("Metric", "Formulation"),
               names_pattern = "(AUC|Cmax)_(Test|Reference)")

cat("\n=== Long Format Data for Boxplot (First 10 rows) ===\n")
print(head(pk_long, 10))


pk_wide_transposed <- pk_long %>%
  pivot_wider(
    id_cols = sim.id,
    names_from = c("Metric", "Formulation"),
    names_sep = "_",
    values_from = "value"
  )

# Display first few rows
head(pk_wide_transposed, 10)

ggplot(pk_long, aes(x = Formulation, y = value, fill = Formulation)) +
  geom_boxplot(outlier.alpha = 0.1) +
  facet_wrap(~Metric, scales = "free") +
  labs(title = "Distribution of AUC and Cmax by Formulation",
       y = "Value", x = "") +
  scale_fill_manual(values = c("Reference" = "lightblue", "Test" = "lightcoral")) +
  theme_minimal()

# Back-transform CI from t-test results
ci_data <- tibble(
  Metric = c("AUC", "Cmax"),
  Lower = exp(c(auc_result$conf.int[1], cmax_result$conf.int[1])),
  Upper = exp(c(auc_result$conf.int[2], cmax_result$conf.int[2])),
  Mean  = exp(c(auc_result$estimate, cmax_result$estimate)),
  Passes = c(auc_passes, cmax_passes)
)

cat("\n=== Confidence Interval Data ===\n")
print(ci_data)

ggplot(ci_data, aes(x = Metric, y = Mean, color = Passes)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper), width = 0.2, size = 1) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 1.25, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 1.0, linetype = "solid", color = "gray", alpha = 0.5) +
  labs(title = "90% Confidence Intervals for Test/Reference Ratios",
       subtitle = "Red dashed lines: Bioequivalence limits (0.8 - 1.25)",
       y = "Geometric Mean Ratio (Test / Reference)", x = "") +
  scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red"), 
                     name = "Passes BE") +
  ylim(0.75, 1.3) +
  theme_minimal()

cat("\n=== SIMULATION COMPLETED SUCCESSFULLY ===\n")
cat("Modify the 'test_parameters' list to enter exact parameter values\n")
cat("Current test parameters used:\n")
cat("- ka:", test_parameters$ka_mean, "\n")
cat("- cl:", test_parameters$cl_mean, "\n") 
cat("- vd:", test_parameters$vd_mean, "\n")
cat("- f:", test_parameters$f_mean, "\n")
cat("- tlag:", test_parameters$tlag_mean, "\n")
