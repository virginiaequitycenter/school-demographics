# Advanced Diploma data from VDOE
# In response to request from Yancey Community Center

# Setup ----
library(tidyverse)
library(janitor)
library(scales)

# 2022-2023 data ----
df <- read_csv("Diplomas and Completion All.csv",
               skip = 3)

advanced <- df %>% 
  clean_names() %>% 
  select(division:cohort, advanced_diplomas) %>% 
  mutate(advanced_diplomas = ifelse(advanced_diplomas == "<",
                                    NA_character_, advanced_diplomas),
         cohort = ifelse(cohort == "<",
                                 NA_character_, cohort),
         advanced_diplomas = as.numeric(advanced_diplomas),
         cohort = as.numeric(cohort),
         percent_advanced = (advanced_diplomas / cohort)*100) %>% 
  filter(subgroup %in% c("All Students", "Asian", "Black",
                         "Hispanic", "Multiple Races", "White")) %>% 
  pivot_wider(names_from = subgroup,
              values_from = c(cohort, advanced_diplomas, percent_advanced)) 

write_csv(advanced, "advanced_diplomas.csv")

advanced_select <- advanced %>% 
  filter(str_detect(division, 
                    "Loudoun|Fairfax|Chesterfield|
                    Fluvanna|Prince William|Louisa|
                    Orange|Greene|Albemarle|
                    Virginia Beach|Charlottesville|Henrico|
                    Buckingham|Richmond City|Waynesboro"))

write_csv(advanced_select, "advanced_diplomas_subset.csv")

# 2020-2021 through 2022-2023 ----
df3 <- read_csv("Diplomas and Completion 3 years.csv",
               skip = 3)

advanced3 <- df3 %>% 
  clean_names() %>% 
  select(year, division:cohort, advanced_diplomas) %>% 
  mutate(advanced_diplomas = ifelse(advanced_diplomas == "<",
                                    NA_character_, advanced_diplomas),
         cohort = ifelse(cohort == "<",
                         NA_character_, cohort),
         advanced_diplomas = as.numeric(advanced_diplomas),
         cohort = as.numeric(cohort),
         percent_advanced = (advanced_diplomas / cohort)*100) %>% 
  filter(subgroup %in% c("All Students", "Asian", "Black",
                         "Hispanic", "Multiple Races", "White"),
         !str_detect(division, "Deaf|Justice")) %>% 
  pivot_wider(names_from = subgroup,
              values_from = c(cohort, advanced_diplomas, percent_advanced)) 

write_csv(advanced3, "advanced_diplomas_3years.csv")

## figure ----
### 2023 comparison, all ----
adv23 <- advanced3 %>% 
  select(year, division, starts_with("percent")) %>% 
  filter(year == 2022) %>% 
  mutate(division = str_remove(division, " Public Schools"),
         order = dense_rank(`percent_advanced_All Students`),
         highlight = case_when(
           division == "Albemarle County" ~ "alb", 
           division == "Charlottesville City" ~ "cvl", 
           TRUE ~ "other")) %>% 
  pivot_longer(starts_with("percent"), names_prefix = "percent_advanced_",
               names_to = "group", values_to = "advanced") 

adv23 %>% 
  filter(group == "All Students") %>% 
  ggplot(aes(y = fct_reorder(division, order), x = advanced, 
             color = highlight)) +
  geom_point(aes(size = highlight)) +
  geom_segment(aes(y = division, yend = division,
                   x = 0, xend = advanced)) +
  labs(x = "Percent Advanced Diplomas", y = "") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_color_manual(values = c("red", "orange", "grey"),
                     guide = "none") +
  scale_size_manual(values = c(2,2,1),
                    guide = "none") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 4))

# by groups...
# separately (for reordering) and patchwork?


### three year trend, select ----
adv2123 <- advanced3 %>% 
  select(year, division, starts_with("percent")) %>% 
  filter(str_detect(division, 
                    "Loudoun|Fairfax|Chesterfield|
                    Fluvanna|Prince William|Louisa|
                    Orange|Greene|Albemarle|
                    Virginia Beach|Charlottesville|Henrico|
                    Buckingham|Richmond City|Waynesboro")) %>% 
  mutate(division = str_remove(division, " Public Schools"),
         order = dense_rank(`percent_advanced_All Students`),
         highlight = ifelse(division == "Albemarle County", "yes", "no")) %>% 
  pivot_longer(starts_with("percent"), names_prefix = "percent_advanced_",
               names_to = "group", values_to = "advanced") 

adv2123_endpoints <- adv2123 %>% 
  filter(group == "All Students", year == 2022)
adv2123 %>% 
  filter(group == "All Students") %>% 
  ggplot(aes(y = advanced, x = year, color = as.factor(division))) +
  geom_line() +
  geom_point() +
  geom_text(data = adv2123_endpoints, size = 3,
            aes(label = division, x = year + 0.3)) + 
  labs(y = "Percent Advanced Diplomas", x = "") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_x_continuous(breaks = c(2020, 2021, 2022),
                     expand = expansion(add = c(0,0.5))) +
  scale_color_manual(values = c("red", "pink", rep("grey",9)),
                     guide = "none") +
  theme_minimal()

# by groups ....
adv2123_endpoints <- adv2123 %>% 
  filter(group != "All Students", year == 2022)
adv2123 %>% 
  filter(group != "All Students") %>% 
  ggplot(aes(y = advanced, x = year, color = as.factor(division))) +
  geom_line() +
  geom_point() +
  geom_text(data = adv2123_endpoints, size = 3,
            aes(label = division, x = year + 0.3)) + 
  facet_wrap(~group, ncol = 1) +
  labs(y = "Percent Advanced Diplomas", x = "") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  scale_x_continuous(breaks = c(2020, 2021, 2022),
                     expand = expansion(add = c(0,0.5))) +
  scale_color_manual(values = c("red", "pink", rep("grey",9)),
                     guide = "none") +
  theme_minimal()
