##############################################################
# School Demographics Map Creation 2023-24                    
# Authors: Asha Muralidharan, Chris Hu     
# GitHub: asha-ec                              
# Last revised: 2024-02-12    
# Summary: A document that creates map for 2023-24 School
#          Demographics for Charlottesville region
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

load("2024schoolmap.Rdata")


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

shape_hs <- st_read(dsn="School Boundary Files 201516/SABS_1516_VA_CvilleRegion_High.shp") 
shape_ms <- st_read(dsn="School Boundary Files 201516/SABS_1516_VA_CvilleRegion_Middle.shp")
shape_ps <- st_read(dsn="School Boundary Files 201516/SABS_1516_VA_CvilleRegion_Primary.shp")


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



map <- leaflet(students2024) %>% 
  addMapPane(name = "boundaries", zIndex = 410)  %>% 
  addMapPane(name = "school_circ", zIndex = 420) %>% 
  addProviderTiles(providers$CartoDB) %>%
  setView(lng = -78.507, lat = 38.0322, zoom = 10) %>%
  addPolygons(data=va_counties, color=districtpalette(va_counties$NAMELSAD), weight=3, fillOpacity=.1) %>%
  addPolygons(data=shape_ps, color = ~leapalette(shape_ps$leaid),fillOpacity=0, weight=2, group= "Elementary School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addPolygons(data=shape_ms, color = ~leapalette(shape_ms$leaid),fillOpacity=0, weight=2, group= "Middle School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addPolygons(data=shape_hs, color = ~leapalette(shape_hs$leaid),fillOpacity=0, weight=2, group= "High School Boundaries",options = leafletOptions(pane = "boundaries")) %>%
  addLayersControl(overlayGroups = c("Elementary School Boundaries", "Middle School Boundaries","High School Boundaries"),options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(lng = ~students2024$Longitude, lat = ~students2024$Latitude, group="Circles",
                    radius = ~students2024$Total_Count*0.009, color = ~districtpalette(students2024$District),
                    weight = 3, fillOpacity = .3, ,options = leafletOptions(pane = "school_circ"),
                    label = paste0("<table style=\"border: 1px solid black\" rules=all >","<tr><th></th><th style=text-align:center>",students2024$School," </th><th style=text-align:center> ",students2024$District,"</th></tr>",
                                   "<tr><th style=text-align:left>Total Count </th><td style=text-align:center>",students2024$Total_Count," </td><td style=text-align:center>",students2024$D_Total_Count,"</td></tr>",
                                   "<tr><th style=text-align:left>White </th><td style=text-align:center>",students2024$White,"%</td><td style=text-align:center>",students2024$D_White,"%</td></tr>",
                                   "<tr><th style=text-align:left>Black </th><td style=text-align:center>",students2024$Black,"%</td><td style=text-align:center>",students2024$D_Black,"%</td></tr>",
                                   "<tr><th style=text-align:left>Hispanic </th><td style=text-align:center>",students2024$Hispanic,"%</td><td style=text-align:center>",students2024$D_Hispanic,"%</td></tr>",
                                   "<tr><th style=text-align:left>Asian </th><td style=text-align:center>",students2024$Asian,"%</td><td style=text-align:center>",students2024$D_Asian,"%</td></tr>",
                                   "<tr><th style=text-align:left>Multiracial </th><td style=text-align:center>",students2024$Multiracial,"%</td><td style=text-align:center>",students2024$D_Multiracial,"%</td></tr>",
                                  "<tr><th style=text-align:left>Econ. Disadvantaged </th><td style=text-align:center>",students2024$Disadvantaged,"%</td><td style=text-align:center>",students2024$D_Disadvantaged,"%</td></tr>",
                                   "<tr><th style=text-align:left>English Learners </th><td style=text-align:center>",students2024$EL,"%</td><td style=text-align:center>",students2024$D_EL,"%</td></tr>",
                                   "</table>") %>% 
                    lapply(htmltools::HTML), 
                    labelOptions = labelOptions(noHide = F, style = list("font-family" = "Arial", "font-size" = "14px"))) %>%
  addLegend("bottomleft", pal=districtpalette, values = students2024$District,title = "",opacity = 0.8) 
  
map

save(students2024, file = "2024schoolmap.Rdata")



