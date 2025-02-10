#Christopher Hu - Summer 2023 
# Updates: Nina Schoonover and Asha Muralidharan January 2024

library(leaflet)
library(leaflegend)
library(dplyr)
library(htmltools)
library(readxl)
library(readr)
library(leafpop)
library(htmlTable)

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

#Let's make a map

#Setting the colors 
districtpalette <- colorFactor(palette= c("#b45248","#50a9a6","#cab2d6","#2f7e9f","#004600","#360021","#002671", "#70B13D", "#E09056", "#0000CA"),
                               domain = c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Nelson County", "Staunton City", "Waynesboro City"))

map <- leaflet(schoolmapdata2023) %>% addProviderTiles(providers$CartoDB) %>%
  setView(lng = -78.507, lat = 38.0322, zoom = 10) %>%
  addCircleMarkers (lng = ~schoolmapdata2023$Longitude, lat = ~schoolmapdata2023$Latitude,
                    radius = ~schoolmapdata2023$Total_Count*0.009, color = ~districtpalette(schoolmapdata2023$District),
                    label = paste0("<table style=\"border: 1px solid black\" rules=all >","<tr><th></th><th style=text-align:center>",schoolmapdata2023$School," </th><th style=text-align:center> ",schoolmapdata2023$District,"</th></tr>",
                                   "<tr><th style=text-align:left>Total Count </th><td style=text-align:center>",schoolmapdata2023$Total_Count," </td><td style=text-align:center>",schoolmapdata2023$D_Total_Count,"</td></tr>",
                                   "<tr><th style=text-align:left>White </th><td style=text-align:center>",schoolmapdata2023$White,"%</td><td style=text-align:center>",schoolmapdata2023$D_White,"%</td></tr>",
                                   "<tr><th style=text-align:left>Black </th><td style=text-align:center>",schoolmapdata2023$Black,"%</td><td style=text-align:center>",schoolmapdata2023$D_Black,"%</td></tr>",
                                   "<tr><th style=text-align:left>Hispanic </th><td style=text-align:center>",schoolmapdata2023$Hispanic,"%</td><td style=text-align:center>",schoolmapdata2023$D_Hispanic,"%</td></tr>",
                                   "<tr><th style=text-align:left>Asian </th><td style=text-align:center>",schoolmapdata2023$Asian,"%</td><td style=text-align:center>",schoolmapdata2023$D_Asian,"%</td></tr>",
                                   "<tr><th style=text-align:left>Multiracial </th><td style=text-align:center>",schoolmapdata2023$Multiracial,"%</td><td style=text-align:center>",schoolmapdata2023$D_Multiracial,"%</td></tr>",
                                   "<tr><th style=text-align:left>Econ. Disadvantaged </th><td style=text-align:center>",schoolmapdata2023$Disadvantaged,"%</td><td style=text-align:center>",schoolmapdata2023$D_Disadvantaged,"%</td></tr>",
                                   "<tr><th style=text-align:left>English Learners </th><td style=text-align:center>",schoolmapdata2023$EL,"%</td><td style=text-align:center>",schoolmapdata2023$D_EL,"%</td></tr>",
                                   "</table>") 
                    %>% lapply(htmltools::HTML), 
                    labelOptions = labelOptions(noHide = F, style = list("font-family" = "Arial", "font-size" = "14px")))

map %>% addLegendFactor(pal = districtpalette,
                        shape = "rect",
                        orientation = "vertical",
                        values = schoolmapdata2023$District,
                        naLabel = "",
                        title = "",
                        opacity = 0.8,
                        width = 20,
                        height = 10, position = "bottomleft")

