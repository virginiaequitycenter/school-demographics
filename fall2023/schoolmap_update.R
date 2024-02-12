# Christopher Hu - Summer 2023 
# Updates: Nina Schoonover and Asha Muralidharan January 2024

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
#Import 2023 data set: all values are percentages unless indicated by count
######################################################

# schoolmapdata2023 <- read_csv("data/schoolmapdata_23.csv")
# View(schoolmapdata2023) 
# schoolmapdata2023$Disadvantaged <- round(schoolmapdata2023$Disadvantaged, 1)
# schoolmapdata2023$Asian <- round(schoolmapdata2023$Asian, 1)
# schoolmapdata2023$Black <- round(schoolmapdata2023$Black, 1)
# schoolmapdata2023$Hispanic <- round(schoolmapdata2023$Hispanic, 1)
# schoolmapdata2023$Multiracial <- round(schoolmapdata2023$Multiracial, 1)
# schoolmapdata2023$White <- round(schoolmapdata2023$White, 1)
# schoolmapdata2023$EL <- round(schoolmapdata2023$EL, 1)
# schoolmapdata2023$D_Disadvantaged <- round(schoolmapdata2023$D_Disadvantaged, 1)
# schoolmapdata2023$D_EL <- round(schoolmapdata2023$D_EL, 1)
# schoolmapdata2023 <- schoolmapdata2023[-89,]

######################################################
# Edit Students 2024 to align with 2023 dataset
######################################################
View(students2024)
# Rename columns to match 
schoolmapdata2024 <- students2024

colnames(schoolmapdata2024) <- c("Year","District","School","Indigenous_Count","Asian_Count","Black_Count","Hispanic_Count","Multiracial_Count",
                                 "White_Count","Pacific_Count","Indigenous","Asian","Black","Hispanic","Multiracial","White","Pacific","Advantaged_Count",
                                 "Disadvantaged_Count","Advantaged","Disadvantaged","Total_Count","Non-EL_Count","EL_Count","Non_EL","EL")
schoolmapdata2024$Asian_Count <- schoolmapdata2024$Asian_Count + schoolmapdata2024$Pacific_Count
schoolmapdata2024$Asian <- schoolmapdata2024$Asian + schoolmapdata2024$Pacific
schoolmapdata2024 <- select(schoolmapdata2024, !c("Pacific_Count","Pacific","Advantaged_Count","Advantaged","Non-EL_Count","Non_EL"))

tot_dist <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_Total_Count = sum(Total_Count))
tot_asian <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_Asian = sum(Asian_Count)*100/sum(Total_Count))
tot_black <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_Black = sum(Black_Count)*100/sum(Total_Count))
tot_hisp <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_Hispanic = sum(Hispanic_Count)*100/sum(Total_Count))
tot_multi <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_Multiracial = sum(Multiracial_Count)*100/sum(Total_Count))
tot_white <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_White = sum(White_Count)*100/sum(Total_Count))
tot_disadv <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_Disadvantaged = sum(Disadvantaged_Count)*100/sum(Total_Count))
tot_el <- schoolmapdata2024 %>% group_by(District) %>% summarize(D_EL = sum(EL_Count)*100/sum(Total_Count))


district <- merge(tot_dist,tot_asian)
district <- merge(district,tot_black)
district <- merge(district,tot_hisp)
district <- merge(district,tot_multi)
district <- merge(district,tot_white)
district <- merge(district,tot_disadv)
district <- merge(district,tot_el)


schoolmapdata2024 <- merge(schoolmapdata2024, district)

schoolmapdata2024$Disadvantaged <- round(schoolmapdata2024$Disadvantaged, 1)
schoolmapdata2024$EL <- round(schoolmapdata2024$EL, 1)
schoolmapdata2024$Asian <- round(schoolmapdata2024$Asian, 1)
schoolmapdata2024$Black <- round(schoolmapdata2024$Black, 1)
schoolmapdata2024$Hispanic <- round(schoolmapdata2024$Hispanic, 1)
schoolmapdata2024$Multiracial <- round(schoolmapdata2024$Multiracial, 1)
schoolmapdata2024$White <- round(schoolmapdata2024$White, 1)
schoolmapdata2024$D_Disadvantaged <- round(schoolmapdata2024$D_Disadvantaged, 1)
schoolmapdata2024$D_EL <- round(schoolmapdata2024$D_EL, 1)
schoolmapdata2024$D_Asian <- round(schoolmapdata2024$D_Asian, 1)
schoolmapdata2024$D_Black <- round(schoolmapdata2024$D_Black, 1)
schoolmapdata2024$D_Hispanic <- round(schoolmapdata2024$D_Hispanic, 1)
schoolmapdata2024$D_Multiracial <- round(schoolmapdata2024$D_Multiracial, 1)
schoolmapdata2024$D_White <- round(schoolmapdata2024$D_White, 1)
schoolmapdata2024 <- schoolmapdata2024[-89,]

latlong <- schoolmapdata2023 %>% select(c("District","School","Latitude","Longitude"))

schoolmapdata2024 <- merge(x=schoolmapdata2024, y=latlong, all.x=TRUE)

#Add in latlongs that aren't included

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Ivy Elementary",38.074917,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Ivy Elementary",-78.507056,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Madison County High",38.364594,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Madison County High",-78.268496,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Madison Primary",38.359348,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Madison Primary",-78.263378,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Waverly Yowell Elementary",38.384902,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Waverly Yowell Elementary",-78.252028,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="William H. Wetsel Middle",38.36389,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="William H. Wetsel Middle",-78.267175,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Lightfoot Elementary",38.246195,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Lightfoot Elementary",-77.953655,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Locust Grove Elementary",38.300355,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Locust Grove Elementary",-77.830787,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Locust Grove Middle",38.315274,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Locust Grove Middle",-77.78051,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Locust Grove Primary",38.29998,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Locust Grove Primary",-77.831971,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Orange County High",38.245245,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Orange County High",-78.096128,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Orange Elementary",38.247816,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Orange Elementary",-78.118295,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Prospect Heights Middle",38.239605,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Prospect Heights Middle",-78.116487,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Unionville Elementary",38.261857,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Unionville Elementary",-77.955105,Longitude))

schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Latitude=ifelse(School=="Gordon-Barbour Elementary",38.138127,Latitude))
schoolmapdata2024 <- schoolmapdata2024 %>% mutate(Longitude=ifelse(School=="Gordon-Barbour Elementary",-78.192894,Longitude))

va_counties <- counties(state="VA", cb=FALSE)
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Charlottesville city", "Charlottesville City", NAMELSAD))
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Staunton city", "Staunton City", NAMELSAD))
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Waynesboro city", "Waynesboro City", NAMELSAD))
va_counties <- va_counties %>% subset(NAMELSAD %in% c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Madison County","Nelson County","Orange County", "Staunton City", "Waynesboro City"))
va_counties <- va_counties %>% st_transform("+init=epsg:4326")

#Let's make a map

#Setting the colors 
districtpalette <- colorFactor(palette= c("#E73F74","#4b4b8f","#3969AC","#7F3C8D","#11A579","#80BA5A","#661150","#008695","#F2B701","#E68310","#CF1C90","#f97b72"),
                               domain = c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Madison County","Nelson County", "Orange County","Staunton City", "Waynesboro City"))

shape_hs <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_High.shp") 
shape_ms <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_Middle.shp")
shape_ps <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_Primary.shp")

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
  addCircleMarkers(lng = ~schoolmapdata2024$Longitude, lat = ~schoolmapdata2024$Latitude, group="Circles",
                    radius = ~schoolmapdata2024$Total_Count*0.009, color = ~districtpalette(schoolmapdata2024$District),
                    weight = 3, fillOpacity = .3, ,options = leafletOptions(pane = "school_circ"),
                    label = paste0("<table style=\"border: 1px solid black\" rules=all >","<tr><th></th><th style=text-align:center>",schoolmapdata2024$School," </th><th style=text-align:center> ",schoolmapdata2024$District,"</th></tr>",
                                   "<tr><th style=text-align:left>Total Count </th><td style=text-align:center>",schoolmapdata2024$Total_Count," </td><td style=text-align:center>",schoolmapdata2024$D_Total_Count,"</td></tr>",
                                   "<tr><th style=text-align:left>White </th><td style=text-align:center>",schoolmapdata2024$White,"%</td><td style=text-align:center>",schoolmapdata2024$D_White,"%</td></tr>",
                                   "<tr><th style=text-align:left>Black </th><td style=text-align:center>",schoolmapdata2024$Black,"%</td><td style=text-align:center>",schoolmapdata2024$D_Black,"%</td></tr>",
                                   "<tr><th style=text-align:left>Hispanic </th><td style=text-align:center>",schoolmapdata2024$Hispanic,"%</td><td style=text-align:center>",schoolmapdata2024$D_Hispanic,"%</td></tr>",
                                   "<tr><th style=text-align:left>Asian </th><td style=text-align:center>",schoolmapdata2024$Asian,"%</td><td style=text-align:center>",schoolmapdata2024$D_Asian,"%</td></tr>",
                                   "<tr><th style=text-align:left>Multiracial </th><td style=text-align:center>",schoolmapdata2024$Multiracial,"%</td><td style=text-align:center>",schoolmapdata2024$D_Multiracial,"%</td></tr>",
                                  "<tr><th style=text-align:left>Econ. Disadvantaged </th><td style=text-align:center>",schoolmapdata2024$Disadvantaged,"%</td><td style=text-align:center>",schoolmapdata2024$D_Disadvantaged,"%</td></tr>",
                                   "<tr><th style=text-align:left>English Learners </th><td style=text-align:center>",schoolmapdata2024$EL,"%</td><td style=text-align:center>",schoolmapdata2024$D_EL,"%</td></tr>",
                                   "</table>") %>% 
                    lapply(htmltools::HTML), 
                    labelOptions = labelOptions(noHide = F, style = list("font-family" = "Arial", "font-size" = "14px"))) %>%
  addLegend("bottomleft", pal=districtpalette, values = schoolmapdata2024$District,title = "",opacity = 0.8) 
  

map 


