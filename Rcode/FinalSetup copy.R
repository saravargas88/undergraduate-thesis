
library(data.table)
library(dplyr)
library(lubridate)
library(readr)
library(stringr)


df_here <- fread("dfs/full_combined.csv")
df_here <- df_here %>%
  rename_with(tolower)%>%
  mutate(ssuid = str_pad(as.character(ssuid), width = 12, side = "left", pad = "0"))

exp <-df_here%>%
 filter(ssuid=='005860332108', epppnum==104)

View(exp)
#names(df_here)

#IDENTIFY CHILDREN AND BMONTH
#BOTH BIOLOGICAL AND ADOPTED


df_here <- df_here %>%
  mutate(date = make_date(rhcalyr, rhcalmn))

# identify children and birth month (bio and adopted)
children_all <- df_here %>%
  filter(tbyear %in% 1995:2012) %>%
  mutate(birth_month = make_date(tbyear, ebmnth, 1)) %>%
  select(ssuid, spanel, epnmom, birth_month)

View(df_here)
View(children_all)

#choose the earlist child not the latest
child_dob_per_mom <- children_all %>%
  group_by(ssuid, epnmom) %>%
  slice_max(birth_month, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(spanel_child = spanel)  # to avoid collision later
child_dob_per_mom %>%
  count(ssuid, epnmom) %>%
  filter(n > 1)  # Should return 0 rows
View(df_here)
View(mothers_with_kids)

#filter mothers
mothers_with_kids <- df_here %>%
  filter(esex == 2, tage %in% 25:45) %>%
  filter(tfipsst %in% c(6, 12, 34, 36, 48)) %>%
  rename(spanel_mother = spanel) %>%
  inner_join(child_dob_per_mom, by = c("ssuid", "epppnum" = "epnmom")) %>%
  filter(spanel_mother == spanel_child)  # Optional but safe

# drop extra panels 
mothers_with_kids <- mothers_with_kids %>%
  mutate(spanel = spanel_mother) %>%
  select(-spanel_mother, -spanel_child)


#make sippid col
sipp_paid_leave_moms <- mothers_with_kids %>%
  group_by(spanel, ssuid, epppnum) %>%
  mutate(sippid = cur_group_id()) %>%
  ungroup() %>%
  arrange(sippid, spanel, swave, srefmon)

sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  group_by(sippid) %>%
  mutate(months = row_number()) %>%
  ungroup()
sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  mutate(date = make_date(year = rhcalyr, month = rhcalmn, day = 1))

View(sipp_paid_leave_moms)


#make the col of relative time from dob to current date 
sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  arrange(ssuid, epppnum, swave, srefmon) %>%
  group_by(ssuid, epppnum) %>%
  mutate(
    birth = round(interval(birth_month, date) / months(1))
  ) %>%
  ungroup()

#DUMMIES FOR REGRESSION: if the birth was seen 1, if birth wasn't observed then 
sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  mutate(birth_seen_f = if_else(birth == 0, 1L, 0L)) %>%
  group_by(sippid) %>%
  mutate(
    birth_seen = max(birth_seen_f, na.rm = TRUE),
    birth_temp = if_else(birth > 0 & birth_seen == 0, birth, NA_integer_),
    birth_temp_valid = ifelse(is.na(birth_temp), Inf, birth_temp),
    ref_month_ns = suppressWarnings(min(birth_temp_valid))        
  ) %>%
  mutate(
    ref_month_ns = ifelse(is.infinite(ref_month_ns), NA_integer_, ref_month_ns)
  ) %>%
  ungroup() %>%
  mutate(
    ref_month = case_when(
      birth == 0 & birth_seen == 1 ~ 1L,
      birth == ref_month_ns & birth_seen == 0 ~ 1L,
      TRUE ~ 0L
    )
  )

View(sipp_paid_leave_moms)

# state factor (treated vs control states)
sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  mutate(state = recode_factor(as.integer(tfipsst),
                               `6`="CA", `34`="NJ", `36`="NY", `12`="FL", `48`="TX"))

sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  mutate(
    post_policy = case_when(
      state == "CA" & birth_month >= as.Date("2004-07-01") ~ 1L,
      state == "NJ" & birth_month >= as.Date("2009-07-01") ~ 1L,
      TRUE ~ 0L
    )
  )

# labor force participation
sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  mutate(
    rm_lfp = if_else(rmesr <= 7, 1L, 0L, missing = NA_integer_),
    rm_lfp = if_else(rmesr == -1, NA_integer_, rm_lfp),
    
    working = if_else(rmesr == 1, 1L, 0L, missing = NA_integer_),
    working = if_else(rmesr == -1, NA_integer_, working),
    
    looking = if_else(rmesr %in% c(5, 6, 7), 1L, 0L, missing = NA_integer_),
    looking = if_else(rmesr == -1, NA_integer_, looking)
  )


#Indicator for erace
sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  group_by(sippid) %>%
  mutate(poc = max(if_else(erace >1, 1, 0))) %>% #if not white 1 if white 0 
  ungroup()

#Indicator for married status : ems 
sipp_paid_leave_moms <- sipp_paid_leave_moms %>%
  group_by(sippid) %>%
  mutate(married = max(if_else(ems ==1, 1, 0))) %>% # if married=1 else not married/separated/divorced then 0 
  ungroup()

write_csv(sipp_paid_leave_moms, "SIPP_Paid_Leave_data_final2.csv")



