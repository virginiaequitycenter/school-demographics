#Nina Schoonover -- Updated February 2025

# script for annual update to the school demographics site

#Load packages
library(tidyverse)
library(dplyr)
library(janitor)
library(leaflet)
library(leaflegend)
library(sf)
library(tigris)

# Pull data from VD0E membership build-a-table

# Query parameters: race.csv
#   school years = 20XX-20XX
#   report level = school
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   schools = all schools
#   race = [all individual races]
#   grades = all grades (aggregated, not individual grades)
#   [everything else] = all students
#
#------Repeat this query to create a full csv for years 2019-20XX (present year)
#   
# Query parameters: disadvantaged.csv
#   school years = 20XX-20XX
#   report level = school
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   schools = all schools
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   disadvantaged = yes, no
#   [everything else] = all students
#
#------Repeat this query to create a full csv for years 2019-20XX (present year)
#
# Query parameters: englishlearner.csv
#   school years = 20XX-20XX
#   report level = school
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange staunton, waynesboro
#   schools = all schools
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   english learner = yes, no [former EL is SELECTED]
#   [everything else] = all students
#
#------Repeat this query to create a full csv for years 2019-20XX (present year)
#
# Build tables for total students by race, disadvantaged, and english learner across district for comparison

# Query parameters: district_race.csv
#   school years = 20XX-20XX
#   report level = division
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   race = [all individual races]
#   grades = all grades (aggregated, not individual grades)
#   [everything else] = all students

#Query parameters: district_disadvantaged.csv
#   school years = 20XX-20XX
#   report level = division
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   disadvantaged = yes, no
#   [everything else] = all students

#Query parameters: district_englishlearner.csv
#   school years = 20XX-20XX
#   report level = division
#   divisions = albemarle, augusta, buckingham, charlottesville, fluvanna, greene, louisa, madison, nelson, orange, staunton, waynesboro
#   race = all races (aggregated, not individual categories)
#   grades = all grades (aggregated, not individual grades)
#   english learner = yes, no [former EL is SELECTED]
#   [everything else] = all students

#set working directory

#read in latlong csv
latlong <- read_csv("school_lat_long.csv")

# read data ----
race_ethn <- read_csv("race.csv") %>% 
  clean_names() %>% 
  select(-c(division_number, school_number,ft_count, pt_count)) %>% 
  rename(count = total_count, school = school_name, division = division_name, year = school_year)

disadvant <- read_csv("disadvantaged.csv") %>% 
  clean_names() %>% 
  select(-c(division_number, school_number, ft_count, pt_count)) %>% 
  rename(count = total_count, school = school_name, division = division_name, year = school_year)

el <- read_csv("englishlearner.csv") %>% 
  clean_names() %>% 
  select(-c(division_number, school_number, ft_count, pt_count)) %>% 
  rename(count = total_count, school = school_name, division = division_name, year = school_year)

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
  mutate(percent = (count/1975)*100,
         percent = round(percent, 1)) %>% 
  pivot_wider(names_from = english_learners_include_former_e_ls, values_from = c(count, percent)) %>% 
  mutate(across(where(is.numeric), ~replace(., is.na(.), 0)))

colnames(el)

names(el) <- c("year", "division", "school", "students", 
               "count_nonel", "count_el", "perc_nonel", "perc_el")

# combine tables and save 

students2024 <- left_join(race_ethn, disadvant, by = c("year", "division", "school")) %>% 
  left_join(., el, by=c("year", "division", "school")) %>%  
  select(-c("students.y", "students.x"))

students2024race <- race_ethn

students2024el <- el

students2024disadvantaged <- disadvant

save(students2024race, file = "2024tablesrace.Rdata")
save(students2024el, file = "2024tablesel.Rdata")
save(students2024disadvantaged, file = "2024tablesdisadvantaged.Rdata")


#Create Totals by district
tot_dist <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_total_count = sum(students))

tot_aian <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_aian = sum(count_aian)*100/sum(students))

tot_asian <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_asian = sum(count_asian)*100/sum(students))

tot_black <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_black = sum(count_black)*100/sum(students))

tot_hisp <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_latinx = sum(count_latinx)*100/sum(students))

tot_multi <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_multiracial = sum(count_multiracial)*100/sum(students))

tot_white <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_white = sum(count_white)*100/sum(students))

tot_nhpi <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_nhpi = sum(count_nhpi)*100/sum(students))

tot_disadv <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_disadvantaged = sum(count_disadv)*100/sum(students))

tot_el <- students2024 %>% 
  group_by(division) %>% 
  summarize(d_el = sum(count_el)*100/sum(students))

district <- tot_dist %>% 
  left_join(tot_aian) %>% 
  left_join(tot_asian) %>% 
  left_join(tot_black) %>% 
  left_join(tot_hisp) %>% 
  left_join(tot_multi) %>% 
  left_join(tot_white) %>% 
  left_join(tot_nhpi) %>% 
  left_join(tot_disadv) %>% 
  left_join(tot_el)

# combine total district data into students2024 and round decimals to one point

students2024 <- merge(students2024, district) %>% 
  mutate(across(where(is.numeric), round, 1))

#combine latitude and longitude into school data

schoolmapdata2024 <- merge(students2024, latlong)

#save Rdata files

save(schoolmapdata2024, file = "2024map.Rdata")

# -----------------------------------------------------------------------------------

# making the map 

va_counties <- counties(state="VA")
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Charlottesville city", "Charlottesville City", NAMELSAD))
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Staunton city", "Staunton City", NAMELSAD))
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Waynesboro city", "Waynesboro City", NAMELSAD))
va_counties <- va_counties %>% subset(NAMELSAD %in% c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Madison County","Nelson County","Orange County", "Staunton City", "Waynesboro City"))

# WARNING MESSAGE - CHECK WITH BETH
va_counties <- va_counties %>% st_transform("+init=epsg:4326")

#Setting the colors 
districtpalette <- colorFactor(palette= c("#E73F74","#4b4b8f","#3969AC","#7F3C8D","#11A579","#80BA5A","#661150","#008695","#F2B701","#E68310","#CF1C90","#f97b72"),
                               domain = c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Madison County","Nelson County", "Orange County","Staunton City", "Waynesboro City"))

shape_hs <- st_read(dsn="SABS 1516 CVille Region/SABS_1516_VA_CvilleRegion_High.shp") 
shape_ms <- st_read(dsn="SABS 1516 CVille Region/SABS_1516_VA_CvilleRegion_Middle.shp")
shape_ps <- st_read(dsn="SABS 1516 CVille Region/SABS_1516_VA_CvilleRegion_Primary.shp")


incl_lea <- c("5100090","5100300","5100540","5100780","5101380","5101710","5102280","5102580","5103690","5103930","5102820")
leapalette <- colorFactor(palette= c("#E73F74","#4b4b8f","#3969AC","#7F3C8D","#11A579","#80BA5A","#661150","#008695","#F2B701","#E68310","#CF1C90","#f97b72"),
                          domain = c("5100090","5100300","5100540","5100780","5101380","5101710","5102280","5102580","5103690","5103930","5102820","5102370"))

shape_hs <- shape_hs %>% subset(stAbbrev=="VA")
shape_hs <- shape_hs %>% subset(leaid %in% incl_lea)
shape_hs <- shape_hs %>% st_transform("+init=epsg:4326")

shape_ms <- shape_ms %>% subset(stAbbrev=="VA")
shape_ms <- shape_ms %>% subset(leaid %in% incl_lea)
shape_ms <- shape_ms %>% st_transform("+init=epsg:4326")

shape_ps <- shape_ps %>% subset(stAbbrev=="VA")
shape_ps <- shape_ps %>% subset(leaid %in% incl_lea)
shape_ps <- shape_ps %>% st_transform("+init=epsg:4326")

map <- leaflet(schoolmapdata2024) %>% 
  addMapPane(name = "boundaries", zIndex = 410)  %>% 
  addMapPane(name = "school_circ", zIndex = 420) %>% 
  addProviderTiles(providers$CartoDB) %>%
  setView(lng = -78.507, lat = 38.0322, zoom = 10) %>%
  addPolygons(data=va_counties, color=districtpalette(va_counties$NAMELSAD), weight=3, fillOpacity=.1) %>%
  addPolygons(data=shape_ps, color = ~leapalette(shape_ps$leaid),fillOpacity=0, weight=2, group= "Elementary School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addPolygons(data=shape_ms, color = ~leapalette(shape_ms$leaid),fillOpacity=0, weight=2, group= "Middle School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addPolygons(data=shape_hs, color = ~leapalette(shape_hs$leaid),fillOpacity=0, weight=2, group= "High School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addLayersControl(overlayGroups = c("Elementary School Boundaries", "Middle School Boundaries","High School Boundaries"),options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(lng = ~schoolmapdata2024$longitude, lat = ~schoolmapdata2024$latitude, group="Circles",
                   radius = ~schoolmapdata2024$students*0.009, color = ~districtpalette(schoolmapdata2024$division),
                   weight = 3, fillOpacity = .3, ,options = leafletOptions(pane = "school_circ"),
                   label = paste0("<table style=\"border: 1px solid black\" rules=all >","<tr><th></th><th style=text-align:center>",schoolmapdata2024$school," </th><th style=text-align:center> ",schoolmapdata2024$division,"</th></tr>",
                                  "<tr><th style=text-align:left>Total Count </th><td style=text-align:center>",schoolmapdata2024$students," </td><td style=text-align:center>",schoolmapdata2024$d_total_count,"</td></tr>",
                                  "<tr><th style=text-align:left>Black </th><td style=text-align:center>",schoolmapdata2024$perc_black,"%</td><td style=text-align:center>",schoolmapdata2024$d_black,"%</td></tr>",
                                  "<tr><th style=text-align:left>Hispanic </th><td style=text-align:center>",schoolmapdata2024$perc_latinx,"%</td><td style=text-align:center>",schoolmapdata2024$d_latinx,"%</td></tr>",
                                  "<tr><th style=text-align:left>Asian </th><td style=text-align:center>",schoolmapdata2024$perc_asian,"%</td><td style=text-align:center>",schoolmapdata2024$d_asian,"%</td></tr>",
                                  "<tr><th style=text-align:left>Multiracial </th><td style=text-align:center>",schoolmapdata2024$perc_multiracial,"%</td><td style=text-align:center>",schoolmapdata2024$d_multiracial,"%</td></tr>",
                                  "<tr><th style=text-align:left>Nat. HI/Pacific </th><td style=text-align:center>",schoolmapdata2024$perc_nhpi,"%</td><td style=text-align:center>",schoolmapdata2024$d_nhpi,"%</td></tr>",
                                  "<tr><th style=text-align:left>White </th><td style=text-align:center>",schoolmapdata2024$perc_white,"%</td><td style=text-align:center>",schoolmapdata2024$d_white,"%</td></tr>",
                                  "<tr><th style=text-align:left>Econ. Disadvantaged </th><td style=text-align:center>",schoolmapdata2024$perc_disadv,"%</td><td style=text-align:center>",schoolmapdata2024$d_disadv,"%</td></tr>",
                                  "<tr><th style=text-align:left>English Learners </th><td style=text-align:center>",schoolmapdata2024$perc_el,"%</td><td style=text-align:center>",schoolmapdata2024$d_el,"%</td></tr>",
                                  "</table>") %>% 
                     lapply(htmltools::HTML), 
                   labelOptions = labelOptions(noHide = F, style = list("font-family" = "Arial", "font-size" = "14px"))) %>%
  addLegend("bottomleft", pal=districtpalette, values = students2024$District,title = "",opacity = 0.8) 

map

## Create full CSV of 2019-20XX year data

fullrace <- read_csv("fullrace_2019_2025.csv")
fullel <- read_csv("fullenglishlearner_2019_2025.csv")
fulldisadv <- read_csv("fulldisadvantaged_2019_2025.csv")

fullcsv <- left_join(fullrace, fullel, by = c("School Year", "Division Name", "School Name")) %>% 
  left_join(., fulldisadv, by=c("School Year", "Division Name", "School Name")) %>%  
  select(-c("students.y", "students.x"))



