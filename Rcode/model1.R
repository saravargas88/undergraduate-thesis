#model 1
library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)
library(fixest)
library(broom)

data <- read_csv("/Users/sara/Desktop/NYUSPRING25/thesisData/dataForRegression.csv")

library(sjPlot)
library(sjmisc)
library(sjlabelled)



##### REGRESSION 1: SIMPLE REGRESSION OF ALL BIRTH MOTHERS POLICY VS NO POLICY EFFECT ON LFP 
#so we add the two way fixed effects 
data <- data%>%
  mutate( 
    time_fe= as.factor(date))
library(fixest)

#m1 HAS ONLY THE POST POLICY INTERACTION AND THE NECESSARY 
m1 <- feols(
  rm_lfp ~ i(lBirth, post_policy, ref = 1)+ i(lBirth, state_code)+ lBirth+ i(lBirth, rhcalyr)|
    sippid + time_fe,
  data = data %>% filter(lBirth %in% c(1, 8:49)),
  weights = ~end_weight,
  cluster = ~sippid
)

iplot(m1, ref.line = 0, main = "M1 Event-Study Estimates", xlab = "Months from Birth")

library(tidyr)
library(broom)
library(stringr)

# EXTRACT COEFFICIENTS FROM REG
betas_df1 <- tidy(m1) %>%
  filter(str_detect(term, "^lBirth::\\d+:post_policy$")) %>%
  mutate(
    time = as.numeric(str_extract(term, "\\d+")) - 25,
    conf.low = estimate - 1.5 * std.error,
    conf.high = estimate + 1.5 * std.error
  )


baseline_df <- tibble(
  time = -24:-18,
  estimate = 0,
  conf.low = 0,
  conf.high = 0
)
betas_df1_full <- bind_rows(baseline_df, betas_df1) %>%
  arrange(time)

#Plot 

ggplot(betas_df1_full, aes(x = time, y = estimate)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = 'blue', alpha = 0.2) +
  geom_point(shape = 17, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, color = "red") +
  labs(
    title = "Figure 3: Model 1 TWFE Including Controls",
    subtitle = "Estimated change in LFP due to paid parental leave policy relative to the baseline\nin months relative to childbirth",
    x = "Months Relative to Birth",
    y = "Change in Labor-Force Participation",
    caption = "Estimates come from a TWFE model with dynamic controls for months in a year and for state. 
The estimated trend shows an increase in LFP during the immediate months around childbirth and a decrease in the long term. 
However, the estimates exhibit large 95% confidence intervals which include 0 and therefore do not provide statistically significant evidence."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.caption = element_text(hjust = 0.5, size = 10) # center the caption
  )
tab_model(
  m1,
  file = "m1_summary.html",
  show.re.var = FALSE,
  show.icc = FALSE,
  show.ngroups = FALSE,
  show.ci = TRUE,
  show.se = TRUE,
  dv.labels = "Model 1: Basic TWFE"
)

#-6 to 6
wald(m1, keep = paste0("lBirth::(", paste(19:31, collapse = "|"), "):post_policy"))

wald(m1, keep = paste0("lBirth::(", paste(25:37, collapse = "|"), "):post_policy"))
wald(m1, keep = "lBirth::\\d+:post_policy")

wald(m1, keep = "lBirth::(25|26|27|28|29|30|31|32|33|34|35|36|37):post_policy")

library(car)
avg_effect <- linearHypothesis(m1, 
                               c("lBirth::24:post_policy = lBirth::25:post_policy", 
                                 "lBirth::25:post_policy = lBirth::26:post_policy",
                                 "lBirth::26:post_policy = lBirth::27:post_policy"))



# Extract and format the interaction coefficients
interaction_df_m1 <- tidy(m1) %>%
  filter(str_detect(term, "^lBirth::\\d+:post_policy$")) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "\\d+")) - 25,
    lower = estimate - 1.96 * std.error,
    upper = estimate + 1.96 * std.error,
    stars = case_when(
      p.value < 0.01 ~ "***",
      p.value < 0.05 ~ "**",
      p.value < 0.1  ~ "*",
      TRUE ~ ""
    )
  ) %>%
  arrange(event_time) %>%
  transmute(
    `Event Time` = event_time,
    Estimate = round(estimate, 3),
    `Std. Error` = round(std.error, 3),
    `p-value` = round(p.value, 3),
    `Significance` = stars,
    `95% Confidence Interval` = paste0("[", round(lower, 3), ", ", round(upper, 3), "]")
  )

#gt table
interaction_table_m1 <- gt(interaction_df_m1) %>%
  tab_header(
    title = "Table 2: Model 1 Post-Policy × Event Time Coefficients"
  ) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(everything())
    
  ) %>%
  tab_source_note("Note: Table 2 reports monthly estimates from Model 1, showing the effect of the post-policy × treatment interaction relative to birth. 
  Pre-policy coefficients support parallel trends; post-policy estimates reflect the policy’s dynamic impact on labor force attachment.
  Significance levels: p < 0.1 (*), p < 0.05 (**), p < 0.01 (***)..
                  ")  %>%

  tab_options(table.font.size = 12)

interaction_table_m1

gtsave(
  interaction_table_m1,
  "interaction_table_m1.png"
)
