
library(dplyr)
library(fixest)
library(broom)
library(ggplot2)
library(readr)


TanyaData <- read_csv("/Users/sara/Desktop/NYUSPRING25/thesisData/dataForRegression.csv")

# marital status based on EMS
TanyaData <- TanyaData %>%
  mutate(
    married = if_else(ems == 1, 1L, 0L)
  )



# isolate a subset of the data for this regression
married_data <- TanyaData %>% filter(married == 1, lBirth %in% c(1, 8:49))
unmarried_data <- TanyaData %>% filter(married == 0, lBirth %in% c(1, 8:49))

# run fixed effects for this subset of married women
mod_married<- feols(
  rm_lfp ~ i(lBirth, post_policy, ref = 1) + i(lBirth, state_code) + i(lBirth, rhcalyr) + lBirth | sippid + rhcalyr,
  data = married_data,
  weights = ~end_weight,
  cluster = ~sippid
)

# Fixed effects for rubset of unmarried
mod_unmarried <- feols(
  rm_lfp ~ i(lBirth, post_policy, ref = 1) + i(lBirth, state_code) + i(lBirth, rhcalyr) + lBirth | sippid + rhcalyr,
  data = unmarried_data,
  weights = ~end_weight,
  cluster = ~sippid
)

summary(mod_unmarried)

library(stringr)
#coefficients for plotting
extract_plot_data <- function(model, group_label) {
  tidy(model) %>%
    filter(str_detect(term, "^lBirth::\\d+:post_policy$")) %>%
    mutate(
      time = as.numeric(str_extract(term, "\\d+")) - 25,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      group = group_label
    )
}

betas_married <- extract_plot_data(mod_married, "Married")
betas_unmarried <- extract_plot_data(mod_unmarried, "Unmarried")

#plotting baselines for each trend
baseline_married <- tibble(
  time = -24:-18,
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  group = "black"
)


baseline_unmarried <- tibble(
  time = -24:-18,
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  group = "black"
)

# combine and plot
plot_data <- bind_rows(betas_married, betas_unmarried)

ggplot(plot_data, aes(x = time, y = estimate, color = group, fill = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Figure 8: Event-Study Estimates by Marital Status",
    subtitle = "Impact of Paid Leave on Labor Force Participation",
    x = "Months Relative to Birth",
    y = "Change in Labor Force Participation", 
    caption= "Event Study coefficient estimates per month relative to childbirth for unmarried and married women.
    relative to the pre-birth baseline period -24 to -17 months relative to childbirth.Point estimates are shown 
    with 95% confidence intervals."
    
  ) +
  theme_minimal(base_size = 14)



make_interaction_table <- function(model, table_title = "Post-Policy × Event-Time Interaction Coefficients",
                                   pattern     = "^lBirth::\\d+:post_policy$",  # updated to match your actual terms
                                   base_period = 25,
                                   ...) {
  
  library(dplyr)
  library(gt)
  library(stringr)
  library(broom)
  
  interaction_df <- tidy(model, ...) %>%
    filter(str_detect(term, pattern)) %>%
    mutate(
      event_time = as.numeric(str_extract(term, "\\d+")) - base_period,
      lower      = estimate - 1.96 * std.error,
      upper      = estimate + 1.96 * std.error,
      stars      = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE           ~ ""
      )
    ) %>%
    arrange(event_time) %>%
    transmute(
      `Event Time`              = event_time,
      Estimate                  = round(estimate, 3),
      `Std. Error`              = round(std.error, 3),
      `p-value`                 = round(p.value, 3),
      Significance              = stars,
      `95% Confidence Interval` = paste0("[", round(lower, 3), ", ", round(upper, 3), "]")
    )
  
  gt(interaction_df) %>%
    tab_header(title = table_title) %>%
    cols_align(align = "center") %>%
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels(everything())
    ) %>%
    fmt_markdown(columns = everything()) %>%
    tab_options(table.font.size = 12)
}

tab1<- make_interaction_table(mod_married, table_title = "Table  Married Individuals")
tab2 <- make_interaction_table(mod_unmarried, table_title = "Table Unmarried Individuals")


#tests: 

# Function to run Wald tests
run_wald <- function(model, months, group_name, label = NULL) {
  terms <- paste0("lBirth::", months, ":post_policy")
  
  # try-catch to catch any wald() failures gracefully
  tryCatch({
    result <- wald(model, terms = terms)
    
    data.frame(
      Group = group_name,
      Window = label %||% paste0("Months ", min(months), " to ", max(months)),
      DF = paste(result$df1, "and", result$df2),
      Stat = round(result$stat, 3),
      p_value = format.pval(result$p, digits = 4, eps = .0001),
      VCOV = result$vcov
    )
  }, error = function(e) {
    warning(paste("Wald test failed for", group_name, label, ":", e$message))
    data.frame(
      Group = group_name,
      Window = label,
      DF = NA,
      Stat = NA,
      p_value = NA,
      VCOV = NA
    )
  })
}

# define windows
windows <- list(
  "−3 to +3" = 22:28,
  "−6 to +6" = 19:31,
  "Full time-frame" = 8:49
)

# define models
models <- list(
  "Married" = mod_married,
  "Unmarried" = mod_unmarried
)

# run Wald tests
wald_results <- bind_rows(
  lapply(names(models), function(g) {
    bind_rows(lapply(names(windows), function(w) {
      run_wald(models[[g]], windows[[w]], g, w)
    }))
  })
)

library(tibble)
library(gt)

# Data for Wald test table 9 on unmarried and married mothers
wald_marital_status <- tibble(
  Group = c("Unmarried", "Unmarried", "Unmarried", "Married", "Married", "Married"),
  `Event-Time Window` = c("−3 to +3", "−6 to +6", "Full 0 to +24", "−3 to +3", "−6 to +6", "Full 0 to +24"),
  `DF (Model)` = c("23 and 16,496", "23 and 16,496", "23 and 16,496", "23 and 45,126", "23 and 45,126", "23 and 45,126"),
  `F Statistic` = c(4.441, 4.441, 4.441, 1.553, 1.553, 1.553),
  `p-value` = c("< 1e-04", "< 1e-04", "< 1e-04", "0.037", "0.037", "0.037")
)

wald_marital_table <- wald_marital_status %>%
  gt() %>%
  tab_header(
    title = "Table 9: Joint Significance of Policy Effects by Marital Status"
  ) %>%
  cols_align("center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_source_note(
    "Note: This table reports Wald tests of the joint significance of the policy interaction coefficients by marital status over specified event-time windows. Each test evaluates the null hypothesis that the policy had no differential effect for the respective group within the window. Standard errors are clustered at the individual level."
  ) %>%
  tab_options(table.font.size = 20)

wald_marital_table


gtsave(wald_marital_table, "wald_table_marital_status.png")















# Create GT table
library(gt)
wald_table <- gt(wald_results) %>%
  tab_header(title = "Table 11: Wald Test Results: Post-Policy Effects for Married vs. Unmarried Subgroups") %>%
  cols_label(
    Group = "Subgroup",
    Window = "Event-Time Window",
    DF = "Degrees of Freedom",
    Stat = "Wald Statistic",
    p_value = "p-Value",
    VCOV = "VCOV Type"
  ) %>%
  cols_align(align = "center", columns = everything()) %>%
  tab_options(table.font.size = 12)

wald_table

unmarried_data <- unmarried_data %>%
  mutate(lBirth_bin = case_when(
    lBirth %in% 1:7    ~ "Ref_(-24 to -18)",
    lBirth %in% 8:10   ~ "-17 to -15",
    lBirth %in% 11:14  ~ "-14 to -11",
    lBirth %in% 15:18  ~ "-10 to -7",
    lBirth %in% 19:21  ~ "-6 to -4",
    lBirth %in% 22:24  ~ "-3 to -1",
    lBirth == 25       ~ "Birth month",
    lBirth %in% 26:28  ~ "+1 to +3",
    lBirth %in% 29:31  ~ "+4 to +6",
    lBirth %in% 32:35  ~ "+7 to +10",
    lBirth %in% 36:42  ~ "+11 to +17",
    lBirth %in% 43:49  ~ "+18 to +24",
    TRUE               ~ NA_character_
  ))

mod_unmarried_binned <- feols(
  rm_lfp ~ post_policy * relevel(factor(lBirth_bin), ref = "Ref_(-24 to -18)") |
    sippid + rhcalyr,
  data = unmarried_data,
  weights = ~end_weight,
  cluster = ~sippid
)
wald(mod_unmarried_binned, 
     terms = c("post_policy:Birth month", 
               "post_policy:+1 to +3", 
               "post_policy:+4 to +6", 
               "post_policy:+7 to +10", 
               "post_policy:+11 to +17", 
               "post_policy:+18 to +24"))


make_wald_table <- function(model, terms, label = "Post-Birth Bins (0 to +24 Months)") {
  test_result <- wald(model, terms = terms)
  
  data.frame(
    `Test Window` = label,
    `DF (Model)` = paste(test_result$df1, "and", test_result$df2),
    `F Statistic` = round(test_result$stat, 3),
    `p-value` = format.pval(test_result$p, digits = 4, eps = .0001)
  ) %>%
    gt() %>%
    tab_header(title = "Wald Test: Joint Significance of Policy Effects") %>%
    cols_label(
      `Test Window` = "Event-Time Interval",
      `DF (Model)` = "Degrees of Freedom",
      `F Statistic` = "F Statistic",
      `p-value` = "p-value"
    ) %>%
    cols_align(align = "center") %>%
    tab_options(table.font.size = 12)
}
postbirth_terms <- c(
  "post_policy:Birth month",
  "post_policy:+1 to +3",
  "post_policy:+4 to +6",
  "post_policy:+7 to +10",
  "post_policy:+11 to +17",
  "post_policy:+18 to +24"
)

# Generate the table
make_wald_table(mod_unmarried_binned, postbirth_terms)


# Define binary indicators for "working" and "looking" in married_data
unmarried_data <- unmarried_data %>%
  mutate(
    working = if_else(rmesr == 1, 1L, 0L, missing = NA_integer_),
    working_log = log(working + 1e-4),
    looking = if_else(rmesr %in% c(5, 6, 7), 1L, 0L, missing = NA_integer_),
    looking_log = log(looking + 1e-4)
  )

# Model: Working
ununmarried_working <- feols(
  working ~ i(lBirth, post_policy, ref = 1) + i(lBirth, rhcalyr) + i(lBirth, state_code) + lBirth | sippid + rhcalyr,
  data = unmarried_data %>% filter(!is.na(working), !is.na(looking)),
  weights = ~end_weight,
  cluster = ~sippid
)

# Model: Looking
ununmarried_working <- feols(
  looking ~ i(lBirth, post_policy, ref = 1) + i(lBirth, rhcalyr) + i(lBirth, state_code) + lBirth | sippid + rhcalyr,
  data = unmarried_data %>% filter(!is.na(working), !is.na(looking)),
  weights = ~end_weight,
  cluster = ~sippid
)

# Extract coefficients for plotting
extract_plot_data <- function(model, label) {
  tidy(model) %>%
    filter(str_detect(term, "^lBirth::\\d+:post_policy$")) %>%
    mutate(
      time = as.numeric(str_extract(term, "\\d+")) - 25,
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      outcome = label
    ) %>%
    select(time, estimate, conf.low, conf.high, outcome)
}

est_working_married <- extract_plot_data(ununmarried_working, "Working")
est_look_married <- extract_plot_data(ununmarried_working, "Looking for work")

# Add baseline
baseline_working <- tibble(
  time = -24:-18,
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  outcome = "Working"
)

baseline_looking <- tibble(
  time = -24:-18,
  estimate = 0,
  conf.low = 0,
  conf.high = 0,
  outcome = "Looking for work"
)

# Combine and plot
plot_df_working_looking_married <- bind_rows(
  est_working_married, est_look_married,
  baseline_working, baseline_looking
)

ggplot(plot_df_working_looking_married, aes(x = time, y = estimate, color = outcome, fill = outcome)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, color = "red", linetype = "dotted") +
  labs(
    title = "Figure 10. Impact of Paid Leave on Labor Force Status for Unmarried Women",
    subtitle = "Decomposition of LFP for Married Women",
    x = "Months relative to birth",
    y = "Change in probability",
    color = "Status",
    fill = "Status"
  ) +
  theme_minimal(base_size = 14)
# Summaries
summary(unmarried_working)
summary(unmarried_working)

# Side-by-side coefficient table
tidy_work <- tidy(unmarried_working) %>%
  filter(str_detect(term, "^lBirth::\\d+:post_policy$")) %>%
  mutate(
    time = as.numeric(str_extract(term, "\\d+")) - 25,
    Estimate_Working = round(estimate, 3),
    SE_Working = round(std.error, 3),
    p_Working = round(p.value, 3)
  ) %>%
  select(time, Estimate_Working, SE_Working, p_Working)

tidy_look <- tidy(unmarried_looking) %>%
  filter(str_detect(term, "^lBirth::\\d+:post_policy$")) %>%
  mutate(
    time = as.numeric(str_extract(term, "\\d+")) - 25,
    Estimate_Looking = round(estimate, 3),
    SE_Looking = round(std.error, 3),
    p_Looking = round(p.value, 3)
  ) %>%
  select(time, Estimate_Looking, SE_Looking, p_Looking)

combined_df <- full_join(tidy_work, tidy_look, by = "time") %>%
  arrange(time)

gt_table <- combined_df %>%
  gt() %>%
  tab_header(title = "Estimates for Married Working vs Looking Models") %>%
  cols_label(
    time = "Event Time",
    Estimate_Working = "Estimate (Working)",
    SE_Working = "SE (Working)",
    p_Working = "p-value (Working)",
    Estimate_Looking = "Estimate (Looking)",
    SE_Looking = "SE (Looking)",
    p_Looking = "p-value (Looking)"
  ) %>%
  fmt_number(columns = everything(), decimals = 3) %>%
  tab_options(table.font.size = 12)

# Show GT table
gt_table

wald(unmarried_working, keep = paste0("lBirth::(", paste(21:28, collapse = "|"), "):post_policy"))
wald(unmarried_working, keep = paste0("lBirth::(", paste(18:31, collapse = "|"), "):post_policy"))
wald(unmarried_working, keep = paste0("lBirth::(", paste(25:37, collapse = "|"), "):post_policy"))
wald(unmarried_working, keep = paste0("lBirth::(", paste(7:49, collapse = "|"), "):post_policy"))
wald(unmarried_working, keep = paste0("lBirth::(", paste(21:28, collapse = "|"), "):post_policy"))


wald(unmarried_looking, keep = paste0("lBirth::(", paste(18:31, collapse = "|"), "):post_policy"))
wald(unmarried_looking, keep = paste0("lBirth::(", paste(25:37, collapse = "|"), "):post_policy"))
wald(unmarried_looking, keep = paste0("lBirth::(", paste(7:49, collapse = "|"), "):post_policy"))
wald(unmarried_looking, keep = paste0("lBirth::(", paste(21:28, collapse = "|"), "):post_policy"))



