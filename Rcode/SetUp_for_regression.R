# Load required libraries
library(haven)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)
library(readr)

mydata <- read_csv("/Users/sara/Desktop/NYUSPRING25/thesisData/SIPP_Paid_Leave_data_final2.csv")

# process and clean original data
mydata <- original_data %>%
  arrange(ssuid, epppnum, spanel, swave, srefmon) %>%
  group_by(spanel, ssuid, epppnum) %>%
  mutate(sippid = cur_group_id()) %>%
  ungroup() %>%
  
  # define state and time variables
  mutate(
    state = case_when(
      tfipsst == 6 ~ "CA",
      tfipsst == 34 ~ "NJ",
      tfipsst == 12 ~ "FL",
      tfipsst == 48 ~ "TX",
      tfipsst == 36 ~ "NY",
      TRUE ~ NA_character_
    ),
    date = make_date(rhcalyr, rhcalmn, 1),
    birth_month_date = ymd("1960-01-01") %m+% months(birth_month),
    birth = interval(birth_month_date, date) %/% months(1),
    
    # labor force participation
    rm_lfp = case_when(
      rmesr <= 7 ~ 1L,
      rmesr > 7 ~ 0L,
      rmesr == -1 ~ NA_integer_,
      TRUE ~ NA_integer_
    ),
    
    # policy indicator
    post_policy = if_else(
      (state == "CA" & birth_month_date >= as.Date("2004-07-01")) |
        (state == "NJ" & birth_month_date >= as.Date("2009-07-01")),
      1L, 0L
    ),
    
    event_time = birth,
    state_code = tfipsst,
    state_time = interaction(tfipsst, rhcalyr, rhcalmn)
  ) %>%
  
  # construct weights
  group_by(sippid) %>%
  mutate(end_date = max(date, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(end_weight_f = if_else(date == end_date, wpfinwgt, NA_real_)) %>%
  group_by(sippid) %>%
  mutate(end_weight = round(max(end_weight_f, na.rm = TRUE))) %>%
  ungroup() %>%
  
  # Bin relative time
  mutate(
    lBirth = case_when(
      birth >= -24 & birth <= -18 ~ 1L,    # Baseline
      birth < -24 ~ 2L,                    # Drop
      birth > 24 ~ 50L,                    # Drop
      TRUE ~ birth + 25L                   # Index shift
    ),
    lBirth = factor(lBirth, levels = as.character(1:50))
  )

# merge subgroup variables from cleaned dataset
subgroup_cols <- mydata[, c("ssuid", "epppnum", "swave", "srefmon", "erace", "ems", "rfoklt18", "thearn")]

mydata <- mydata %>%
  left_join(
    subgroup_cols,
    by = c("ssuid", "epppnum", "swave", "srefmon")
  )

# filter for CA/NJ and birth window
panel_a_data <- mydata %>%
  filter(state %in% c("CA", "NJ")) %>%
  filter(birth >= -24, birth <= 24) %>%
  mutate(
    policy_period = case_when(
      state == "CA" & birth_month_date >= as.Date("2004-07-01") ~ "After",
      state == "NJ" & birth_month_date >= as.Date("2009-07-01") ~ "After",
      TRUE ~ "Before"
    )
  )

# *********Figure 1: LFP before/after childbirth by policy period  *******
lfp_levels <- panel_a_data %>%
  group_by(policy_period, birth) %>%
  summarise(mean_lfp = mean(rm_lfp, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = policy_period, values_from = mean_lfp) %>%
  mutate(
    diff = After - Before,
    time = birth
  ) %>%
  filter(!is.na(Before), !is.na(After))

ggplot(lfp_levels, aes(x = time)) +
  geom_ribbon(aes(ymin = Before, ymax = After, fill = "Difference Range"), alpha = 0.2) +
  geom_line(aes(y = Before, color = "Before"), linetype = "dashed", size = 1) +
  geom_point(aes(y = Before, color = "Before"), shape = 5) +
  geom_line(aes(y = After, color = "After"), size = 1) +
  geom_point(aes(y = After, color = "After"), shape = 18) +
  geom_line(aes(y = diff, color = "Difference"), linetype = "dotted", size = 1) +
  geom_vline(xintercept = 0, color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray60") +
  scale_y_continuous(
    name = "Labor-force participation",
    sec.axis = sec_axis(~ ., name = "After - Before LFP", breaks = seq(-0.05, 0.1, 0.05))
  ) +
  scale_color_manual(values = c("Before" = "#1f77b4", "After" = "#2ca02c", "Difference" = "darkorange")) +
  scale_fill_manual(values = c("Difference Range" = "gray70")) +
  labs(
    title = "Figure 1. LFP Around Birth in CA and NJ",
    subtitle = "Before and After Policy",
    x = "Months Relative to Birth",
    color = "",
    fill = "",
    caption = paste(
      "The figure shows labor force participation (LFP) trends in California and New Jersey around childbirth,",
      "comparing periods before and after the implementation of paid parental leave policies. Green = After,",
      "Blue = Before, Orange = Difference. Shaded area = range of difference."
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0, size = 10)
  )

# subset for trend analysis
df_subset1 <- lfp_levels %>%
  filter(time >= -6, time <= 0) %>%
  select(time, Before, After) %>%
  pivot_longer(cols = c("Before", "After"), names_to = "group", values_to = "lfp") %>%
  rename(month = time)

# Linear trends
model_before <- lm(lfp ~ month, data = filter(df_subset1, group == "Before"))
model_after  <- lm(lfp ~ month, data = filter(df_subset1, group == "After"))

#  slopes
summary(model_before)$coefficients["month", ]
summary(model_after)$coefficients["month", ]


write_csv(mydata, "dataForRegression.csv")
