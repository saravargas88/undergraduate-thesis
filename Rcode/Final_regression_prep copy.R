
library(tidyverse)
library(lubridate)
library(fixest)
library(broom)
library(stargazer)


mydata <- read_csv("Desktop/NYUSPRING25/thesisData/SIPP_Paid_Leave_data_final2.csv")



#event study indexing : Birth
mydata <- mydata %>%
  mutate(
    Birth = case_when(
      is.na(birth) ~ NA_integer_,
      birth <= -25 ~ 50L,
      birth > 24   ~ 51L,
      TRUE         ~ birth + 25L
    ) %>% factor()
  )

#PLOT 1: EVENT STUDY
mydata_event <- mydata %>%
  filter(state %in% c("CA", "NJ"),
         birth >= -24, birth <= 24,
         post_policy %in% c(0, 1),
         !is.na(rm_lfp))

model <- feols(
  rm_lfp ~ i(Birth, post_policy, ref = 24) | sippid + date + state,
  data = mydata_event,
  cluster = ~sippid
)

coef_df <- tidy(model) %>%
  filter(str_detect(term, "^Birth::")) %>%
  mutate(
    #extract Birth index from 'Birth::XX:0' and convert to event_time
    birth_index = as.integer(str_extract(term, "(?<=Birth::)\\d+")),
    event_time = birth_index - 25,
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )

ggplot(coef_df, aes(x = event_time, y = estimate)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Event Study: Impact of Policy on Labor Force Participation",
       x = "Months Since Childbirth",
       y = "Effect on Labor Force Participation") +
  theme_minimal()


## PLOT 2 : 
## descriptive visualization of average labor force participation (LFP) over time relative to childbirth, split by policy exposure. 
mydata_filtered <- mydata %>% filter(state %in% c("CA", "NJ"), birth >= -24, birth <= 24)

grouped_avg <- mydata_filtered %>%
  group_by(birth, post_policy) %>%
  summarise(mean_lfp = mean(rm_lfp, na.rm = TRUE), .groups = "drop") %>%
  mutate(group = if_else(post_policy == 1, "Post-policy (Treated)", "Pre-policy (Control)"))

diff_df <- grouped_avg %>%
  pivot_wider(names_from = group, values_from = mean_lfp) %>%
  mutate(diff = `Post-policy (Treated)` - `Pre-policy (Control)`)

ggplot(grouped_avg, aes(x = birth, y = mean_lfp, color = group)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_line(data = diff_df, aes(x = birth, y = diff),
            inherit.aes = FALSE, color = "orange", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    title = "Labor Force Participation Around Childbirth in CA/NJ",
    subtitle = "Treated vs Control Trends + Policy Difference (orange)",
    x = "Months since childbirth", y = "Mean LFP (rm_lfp)", color = "Group"
  ) +
  theme_minimal()

##DYNAMIC DID: 
mydata_did <- mydata %>%
  filter(state %in% c("CA", "NJ"),
         birth >= -24, birth <= 24,
         post_policy %in% c(0, 1),
         !is.na(rm_lfp))  

table(mydata_did$rm_lfp)

event_model <- feols(
  rm_lfp ~ i(birth, post_policy, ref = -1) | sippid + date,  # event-study dummies interacted with treatment
  data = mydata_did,
  cluster = ~sippid # clusters standard errors at the individual level
)

event_df <- tidy(event_model) %>% #EXTRACT COEFFICIENTS AND PREP PLOTTING
  filter(str_detect(term, "birth::")) %>%
  mutate(
    event_time = as.integer(str_extract(term, "(?<=birth::)-?\\d+")),
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )
table(event_df$event_time)

#OLS MODEL WITHOUT FIXED EFFECTS: 
lm_model <- lm(rm_lfp ~ i(birth, post_policy, ref = -1), data = mydata_did)
summary(lm_model)

mydata_did <- mydata_did %>%
  mutate(
    state = factor(state),
    date = factor(date),  # assuming it's already like "2020-03"
    state_date = interaction(state, date, drop = TRUE)
  )
#FIXED EFFECTS OLS MODEL
event_model_fe <- feols(
  rm_lfp ~ i(birth, post_policy, ref = -1) | sippid +date+ interaction(state, date),  # drop date FE for now
  data = mydata_did,
  cluster = ~sippid
)

#PLOT EVENT MODEL FE
event_df <- tidy(event_model_fe) %>%
  filter(str_detect(term, "birth::")) %>%
  mutate(
    event_time = as.integer(str_extract(term, "(?<=birth::)-?\\d+")),
    ci_low = estimate - 1.96 * std.error,
    ci_high = estimate + 1.96 * std.error
  )

ggplot(event_df, aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  labs(
    title = "Dynamic DiD: Paid Leave Policy's Causal Effect on LFP",
    subtitle = "Relative to month -1 (with individual fixed effects)",
    x = "Months since childbirth",
    y = "Causal effect estimate"
  ) +
  theme_minimal(base_size = 13)


