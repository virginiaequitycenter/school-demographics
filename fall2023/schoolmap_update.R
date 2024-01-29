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

#Import data set: all values are percentages unless indicated by count
schoolmapdata2023 <- read_csv("data/schoolmapdata_23.csv")
View(schoolmapdata2023) 
schoolmapdata2023$Disadvantaged <- round(schoolmapdata2023$Disadvantaged, 1)
schoolmapdata2023$Asian <- round(schoolmapdata2023$Asian, 1)
schoolmapdata2023$Black <- round(schoolmapdata2023$Black, 1)
schoolmapdata2023$Hispanic <- round(schoolmapdata2023$Hispanic, 1)
schoolmapdata2023$Multiracial <- round(schoolmapdata2023$Multiracial, 1)
schoolmapdata2023$White <- round(schoolmapdata2023$White, 1)
schoolmapdata2023$EL <- round(schoolmapdata2023$EL, 1)
schoolmapdata2023$D_Disadvantaged <- round(schoolmapdata2023$D_Disadvantaged, 1)
schoolmapdata2023$D_EL <- round(schoolmapdata2023$D_EL, 1)

schoolmapdata2023 <- schoolmapdata2023[-89,]

va_counties <- counties(state="VA", cb=FALSE)
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Charlottesville city", "Charlottesville City", NAMELSAD))
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Staunton city", "Staunton City", NAMELSAD))
va_counties <- va_counties %>% mutate(NAMELSAD= ifelse(NAMELSAD=="Waynesboro city", "Waynesboro City", NAMELSAD))
va_counties <- va_counties %>% subset(NAMELSAD %in% c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Nelson County", "Staunton City", "Waynesboro City"))
va_counties <- va_counties %>% st_transform("+init=epsg:4326")

#Let's make a map

#Setting the colors 
districtpalette <- colorFactor(palette= c("#b45248","#50a9a6","#cab2d6","#2f7e9f","#004600","#360021","#002671", "#70B13D", "#E09056", "#0000CA"),
                               domain = c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Nelson County", "Staunton City", "Waynesboro City"))

shape_hs <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_High.shp") 
shape_ms <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_Middle.shp")
shape_ps <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_Primary.shp")

incl_lea <- c("5100090","5100300","5100540","5100780","5101380","5101710","5102280","5102580","5103690","5103930")
leapalette <- colorFactor(palette= c("#b45248","#50a9a6","#cab2d6","#2f7e9f","#004600","#360021","#002671", "#70B13D", "#E09056", "#0000CA"),
                               domain = c("5100090","5100300","5100540","5100780","5101380","5101710","5102280","5102580","5103690","5103930"))


shape_hs <- shape_hs %>% subset(stAbbrev=="VA")
shape_hs <- shape_hs %>% subset(leaid %in% incl_lea)
shape_hs <- shape_hs %>% st_transform("+init=epsg:4326")

shape_ms <- shape_ms %>% subset(stAbbrev=="VA")
shape_ms <- shape_ms %>% subset(leaid %in% incl_lea)
shape_ms <- shape_ms %>% st_transform("+init=epsg:4326")


shape_ps <- shape_ps %>% subset(stAbbrev=="VA")
shape_ps <- shape_ps %>% subset(leaid %in% incl_lea)
shape_ps <- shape_ps %>% st_transform("+init=epsg:4326")



map <- leaflet(schoolmapdata2023) %>% 
  addMapPane(name = "boundaries", zIndex = 410)  %>% 
  addMapPane(name = "school_circ", zIndex = 420) %>% 
  addProviderTiles(providers$CartoDB) %>%
  setView(lng = -78.507, lat = 38.0322, zoom = 10) %>%
  addPolygons(data=va_counties, color=districtpalette(va_counties$NAMELSAD), weight=3, fillOpacity=.1) %>%
  addPolygons(data=shape_ps, color = ~leapalette(shape_ps$leaid),fillOpacity=0, weight=2, group= "Elementary School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addPolygons(data=shape_ms, color = ~leapalette(shape_ms$leaid),fillOpacity=0, weight=2, group= "Middle School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addPolygons(data=shape_hs, color = ~leapalette(shape_hs$leaid),fillOpacity=0, weight=2, group= "High School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addLayersControl(overlayGroups = c("Elementary School Boundaries", "Middle School Boundaries","High School Boundaries"),options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(lng = ~schoolmapdata2023$Longitude, lat = ~schoolmapdata2023$Latitude, group="Circles",
                    radius = ~schoolmapdata2023$Total_Count*0.009, color = ~districtpalette(schoolmapdata2023$District),
                    weight = 3, fillOpacity = .3, ,options = leafletOptions(pane = "school_circ"),
                    label = paste0("<table style=\"border: 1px solid black\" rules=all >","<tr><th></th><th style=text-align:center>",schoolmapdata2023$School," </th><th style=text-align:center> ",schoolmapdata2023$District,"</th></tr>",
                                   "<tr><th style=text-align:left>Total Count </th><td style=text-align:center>",schoolmapdata2023$Total_Count," </td><td style=text-align:center>",schoolmapdata2023$D_Total_Count,"</td></tr>",
                                   "<tr><th style=text-align:left>White </th><td style=text-align:center>",schoolmapdata2023$White,"%</td><td style=text-align:center>",schoolmapdata2023$D_White,"%</td></tr>",
                                   "<tr><th style=text-align:left>Black </th><td style=text-align:center>",schoolmapdata2023$Black,"%</td><td style=text-align:center>",schoolmapdata2023$D_Black,"%</td></tr>",
                                   "<tr><th style=text-align:left>Hispanic </th><td style=text-align:center>",schoolmapdata2023$Hispanic,"%</td><td style=text-align:center>",schoolmapdata2023$D_Hispanic,"%</td></tr>",
                                   "<tr><th style=text-align:left>Asian </th><td style=text-align:center>",schoolmapdata2023$Asian,"%</td><td style=text-align:center>",schoolmapdata2023$D_Asian,"%</td></tr>",
                                   "<tr><th style=text-align:left>Multiracial </th><td style=text-align:center>",schoolmapdata2023$Multiracial,"%</td><td style=text-align:center>",schoolmapdata2023$D_Multiracial,"%</td></tr>",
                                  "<tr><th style=text-align:left>Econ. Disadvantaged </th><td style=text-align:center>",schoolmapdata2023$Disadvantaged,"%</td><td style=text-align:center>",schoolmapdata2023$D_Disadvantaged,"%</td></tr>",
                                   "<tr><th style=text-align:left>English Learners </th><td style=text-align:center>",schoolmapdata2023$EL,"%</td><td style=text-align:center>",schoolmapdata2023$D_EL,"%</td></tr>",
                                   "</table>") %>% 
                    lapply(htmltools::HTML), 
                    labelOptions = labelOptions(noHide = F, style = list("font-family" = "Arial", "font-size" = "14px"))) %>%
  addLegend("bottomleft", pal=districtpalette, values = schoolmapdata2023$District,title = "",opacity = 0.8) 
  

map 




