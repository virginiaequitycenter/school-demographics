##############################################################
# 2024 School Demographics File Prep                    
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2024-02-12    
# Summary: A document that removes unnecessary shapefiles from
#          SABS Survey 2015-16 to prepare for use in 
#          updated school demo maps of Charlottesville Area 
##############################################################


library(leaflet)
library(leaflegend)
library(dplyr)
library(htmltools)
library(readxl)
library(readr)
library(leafpop)
library(htmlTable)
library(sf)
library(tigris)
library(tibble)

######################################################
# Edit Students 2024 to align with 2023 dataset
######################################################
# Rename columns to match 

schoolmapdata2023 <- read_csv("data/schoolmapdata_23.csv")
save(schoolmapdata2023, file = "2023schoolmap.Rdata")

load("~/Git_Projects/school-demographics/fall2023/2024demographictables.Rdata")


colnames(students2024) <- c("Year","District","School","Indigenous_Count","Asian_Count","Black_Count","Hispanic_Count","Multiracial_Count",
                                 "White_Count","Pacific_Count","Indigenous","Asian","Black","Hispanic","Multiracial","White","Pacific","Advantaged_Count",
                                 "Disadvantaged_Count","Advantaged","Disadvantaged","Total_Count","Non-EL_Count","EL_Count","Non_EL","EL")
students2024$Asian_Count <- students2024$Asian_Count + students2024$Pacific_Count
students2024$Asian <- students2024$Asian + students2024$Pacific
students2024 <- select(students2024, !c("Pacific_Count","Pacific","Advantaged_Count","Advantaged","Non-EL_Count","Non_EL"))

tot_dist <- students2024 %>% group_by(District) %>% summarize(D_Total_Count = sum(Total_Count))
tot_asian <- students2024 %>% group_by(District) %>% summarize(D_Asian = sum(Asian_Count)*100/sum(Total_Count))
tot_black <- students2024 %>% group_by(District) %>% summarize(D_Black = sum(Black_Count)*100/sum(Total_Count))
tot_hisp <- students2024 %>% group_by(District) %>% summarize(D_Hispanic = sum(Hispanic_Count)*100/sum(Total_Count))
tot_multi <- students2024 %>% group_by(District) %>% summarize(D_Multiracial = sum(Multiracial_Count)*100/sum(Total_Count))
tot_white <- students2024 %>% group_by(District) %>% summarize(D_White = sum(White_Count)*100/sum(Total_Count))
tot_disadv <- students2024 %>% group_by(District) %>% summarize(D_Disadvantaged = sum(Disadvantaged_Count)*100/sum(Total_Count))
tot_el <- students2024 %>% group_by(District) %>% summarize(D_EL = sum(EL_Count)*100/sum(Total_Count))


district <- merge(tot_dist,tot_asian)
district <- merge(district,tot_black)
district <- merge(district,tot_hisp)
district <- merge(district,tot_multi)
district <- merge(district,tot_white)
district <- merge(district,tot_disadv)
district <- merge(district,tot_el)


students2024 <- merge(students2024, district)

students2024$Disadvantaged <- round(students2024$Disadvantaged, 1)
students2024$EL <- round(students2024$EL, 1)
students2024$Asian <- round(students2024$Asian, 1)
students2024$Black <- round(students2024$Black, 1)
students2024$Hispanic <- round(students2024$Hispanic, 1)
students2024$Multiracial <- round(students2024$Multiracial, 1)
students2024$White <- round(students2024$White, 1)
students2024$D_Disadvantaged <- round(students2024$D_Disadvantaged, 1)
students2024$D_EL <- round(students2024$D_EL, 1)
students2024$D_Asian <- round(students2024$D_Asian, 1)
students2024$D_Black <- round(students2024$D_Black, 1)
students2024$D_Hispanic <- round(students2024$D_Hispanic, 1)
students2024$D_Multiracial <- round(students2024$D_Multiracial, 1)
students2024$D_White <- round(students2024$D_White, 1)
students2024 <- students2024[-89,]

latlong <- schoolmapdata2023 %>% select(c("District","School","Latitude","Longitude"))

students2024 <- merge(x=students2024, y=latlong, all.x=TRUE)

#Add in latlongs that aren't included

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Ivy Elementary",38.0812,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Ivy Elementary",-78.5981,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Madison County High",38.364594,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Madison County High",-78.268496,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Madison Primary",38.359348,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Madison Primary",-78.263378,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Waverly Yowell Elementary",38.384902,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Waverly Yowell Elementary",-78.252028,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="William H. Wetsel Middle",38.36389,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="William H. Wetsel Middle",-78.267175,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Lightfoot Elementary",38.246195,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Lightfoot Elementary",-77.953655,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Locust Grove Elementary",38.300355,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Locust Grove Elementary",-77.830787,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Locust Grove Middle",38.315274,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Locust Grove Middle",-77.78051,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Locust Grove Primary",38.29998,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Locust Grove Primary",-77.831971,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Orange County High",38.245245,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Orange County High",-78.096128,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Orange Elementary",38.247816,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Orange Elementary",-78.118295,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Prospect Heights Middle",38.239605,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Prospect Heights Middle",-78.116487,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Unionville Elementary",38.261857,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Unionville Elementary",-77.955105,Longitude))

students2024 <- students2024 %>% mutate(Latitude=ifelse(School=="Gordon-Barbour Elementary",38.138127,Latitude))
students2024 <- students2024 %>% mutate(Longitude=ifelse(School=="Gordon-Barbour Elementary",-78.192894,Longitude))

save(students2024, file = "2024schoolmap.Rdata")
