#Nina Schoonover -- January 2023

# updating school demographics tables to include race, economically disadvantaged, and english learners
# adding all local districts: albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro) 

#Load packages
library(tidyverse)
library(dplyr)
library(janitor)

#VD0E membership build-a-table
# Query parameters: race.csv
#   school years = 2023-2024
#   report level = school
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   schools = all schools
#   race = [all individual races]
#   grades = all grades (aggregated, not individual grades)
#   [everything else] = all students
#   
# Query parameters: disadvantaged.csv
#   school years = 2023-2024
#   report level = school
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   schools = all schools
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   disadvantaged = yes, no
#   [everything else] = all students

# Query parameters: englishlearner.csv
#   school years = 2023-2024
#   report level = school
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange staunton, waynesboro
#   schools = all schools
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   english learner = yes, no [former EL not selected]
#   [everything else] = all students

# Build tables for total students by race, disadvantaged, and english learner across district for comparison

# Query parameters: district_race.csv
#   school years = 2023-2024
#   report level = division
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   race = [all individual races]
#   grades = all grades (aggregated, not individual grades)
#   [everything else] = all students

#Query parameters: district_disadvantaged.csv
#   school years = 2023-2024
#   report level = division
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   disadvantaged = yes, no
#   [everything else] = all students

#Query parameters: district_englishlearner.csv
#   school years = 2023-2024
#   report level = division
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   english learner = yes, no [former EL not selected]
#   [everything else] = all students

#set working directory

# read data ----
race_ethn <- read_csv("data/race.csv") %>% 
  clean_names() %>% 
  select(-c(division_number, school_number,full_time_count_all_grades, part_time_count_all_grades)) %>% 
    rename(count = total_count, school = school_name, division = division_name)

disadvant <- read_csv("data/disadvantaged.csv") %>% 
  clean_names() %>% 
  select(-c(division_number, school_number, full_time_count_all_grades, part_time_count_all_grades)) %>% 
    rename(count = total_count, school = school_name, division = division_name)

el <- read_csv("data/englishlearner.csv") %>% 
  clean_names() %>% 
  select(-c(division_number, school_number, full_time_count_all_grades, part_time_count_all_grades)) %>% 
    rename(count = total_count, school = school_name, division = division_name)

districtbyrace <- read_csv("data/district_race.csv") %>% 
  clean_names() %>% 
  select(-c(division_number, full_time_count_all_grades, part_time_count_all_grades)) %>% 
   rename(count = total_count, division = division_name)

#build tables
race_ethn <- race_ethn %>% 
  group_by(school) %>% 
  mutate(num_students = sum(count)) %>% 
  ungroup() %>% 
  mutate(percent = (count/num_students)*100, percent = round(percent, 1)) %>% 
  pivot_wider(names_from = race, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

colnames(race_ethn)

names(race_ethn) <- c("year", "division", "school", "students", "count_aian",
                     "count_asian", "count_black",
                     "count_latinx", "count_multiracial", "count_white",
                     "count_nhpi", "perc_aian", "perc_asian", 
                     "perc_black", "perc_latinx", "perc_multiracial",
                     "perc_white", "perc_nhpi" )

disadvant <- disadvant %>%  
  mutate(count = if_else(count == "<", "9", count), 
         count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  group_by(school) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(percent = (count/num_students)*100,
         percent = round(percent, 1)) %>% 
  pivot_wider(names_from = disadvantaged, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

colnames(disadvant)

names(disadvant) <- c("year", "division", "school", "students", 
                    "count_adv", "count_disadv", "perc_adv", "perc_disadv")

el <- el %>%  
  mutate(count = if_else(count == "<", "9", count), 
         count = str_remove(count, ","),
         count = as.integer(count)) %>% 
  group_by(school) %>% 
  mutate(num_students = sum(count, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(percent = (count/num_students)*100,
         percent = round(percent, 1)) %>% 
  pivot_wider(names_from = english_learners, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

colnames(el)

names(el) <- c("year", "division", "school", "students", 
                      "count_nonel", "count_el", "perc_nonel", "perc_el")

# combine tables and save 

students2024race <- race_ethn

students2024el <- el

students2024disadvantaged <- disadvant

students2024 <- left_join(race_ethn, disadvant, by = c("year", "division", "school")) %>% 
  left_join(., el, by=c("year", "division", "school")) %>%  
  select(-c("students.y", "students.x"))


save(students2024race, file = "2024demographictablesrace.Rdata")
save(students2024el, file = "2024demographictablesel.Rdata")
save(students2024disadvantaged, file = "2024demographictablesdisadvantaged.Rdata")

