library(tidyverse)
library(janitor)
library(DT)

# data from VDOE Fall Membership Build-a-Table
# https://p1pe.doe.virginia.gov/apex_captcha/home.do?apexTypeId=304

# read data ----
race_ethn <- read_csv("fall_membership_race.csv") %>% 
  clean_names()
disadvant <- read_csv("fall_membership_disadvantaged.csv") %>% 
  clean_names()

# what schools
race_ethn %>% filter(school_year == "2019-2020") %>% 
  distinct(school_name)
disadvant %>% filter(school_year == "2019-2020") %>% 
  distinct(school_name)

race_ethn %>% filter(school_year == "2020-2021") %>% 
  distinct(school_name)
disadvant %>% filter(school_year == "2020-2021") %>% 
  distinct(school_name)
# Ablemarle County community public charter gone

race_ethn %>% filter(school_year == "2021-2022") %>% 
  distinct(school_name)
disadvant %>% filter(school_year == "2021-2022") %>% 
  distinct(school_name)
# sutherland becomes lakeside; 
# community lab school added, murray subtracted (community lab is formerly murray)

# tables ----
# separately by year
# fall 2021
race2022 <- race_ethn %>% filter(school_year == "2021-2022") %>% 
  select(-c(division_number, school_number, full_time_count_all_grades, part_time_count_all_grades)) %>% 
  rename(count = total_count) %>% 
  group_by(school_name) %>% 
  mutate(num_students = sum(count)) %>% 
  ungroup() %>% 
  mutate(percent = (count/num_students)*100,
         percent = round(percent, 1)) %>% 
  pivot_wider(names_from = race, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

names(race2022) <- c("year", "division", "school", "students", 
                     "count_aian", "count_asian", "count_black",
                     "count_latinx", "count_multi", "count_white",
                     "count_nhpi", "perc_aian", "perc_asian", 
                     "perc_black", "perc_latinx", "perc_multi",
                     "perc_white", "perc_nhpi")

dis2022 <- disadvant %>% filter(school_year == "2021-2022") %>% 
  mutate(count = if_else(total_count == "<", "9", total_count), # based on total students in race2022
         count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  select(-c(division_number, school_number, full_time_count_all_grades, part_time_count_all_grades, total_count)) %>% 
  group_by(school_name) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(percent = (count/num_students)*100,
         percent = round(percent, 1)) %>% 
  pivot_wider(names_from = disadvantaged, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

names(dis2022) <- c("year", "division", "school", "students", 
                    "count_adv", "count_disadv", "perc_adv", "perc_disadv")

# fall 2019 (pre-covid)
race2020 <- race_ethn %>% filter(school_year == "2019-2020") %>% 
  select(-c(division_number, school_number, full_time_count_all_grades, part_time_count_all_grades)) %>% 
  rename(count = total_count) %>% 
  group_by(school_name) %>% 
  mutate(num_students = sum(count)) %>% 
  ungroup() %>% 
  mutate(percent = (count/num_students)*100,
         percent = round(percent, 1)) %>% 
  pivot_wider(names_from = race, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

names(race2020) <- c("year", "division", "school", "students", 
                     "count_aian", "count_asian", "count_black",
                     "count_latinx", "count_multi", "count_white",
                     "count_nhpi", "perc_aian", "perc_asian", 
                     "perc_black", "perc_latinx", "perc_multi",
                     "perc_white", "perc_nhpi")

dis2020 <- disadvant %>% filter(school_year == "2019-2020") %>% 
  mutate(count = str_remove(total_count, ","),
         count = as.integer(count)) %>% 
  select(-c(division_number, school_number, full_time_count_all_grades, part_time_count_all_grades, total_count)) %>% 
  group_by(school_name) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(percent = (count/num_students)*100,
         percent = round(percent, 1)) %>% 
  pivot_wider(names_from = disadvantaged, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0))) 

names(dis2020) <- c("year", "division", "school", "students", 
                    "count_adv", "count_disadv", "perc_adv", "perc_disadv")

# combine tables and save
students2022 <- left_join(race2022, dis2022)
students2020 <- left_join(race2020, dis2020)

save(students2020, students2022, file = "students.Rdata")

# # Colors
# library(ghibli)
# library(scales)
# show_col(ghibli_palettes$TotoroLight)
# show_col(ghibli_palettes$KikiLight)
# show_col(ghibli_palettes$SpiritedLight)
# show_col(ghibli_palettes$MononokeLight)
# show_col(ghibli_palettes$PonyoLight)
