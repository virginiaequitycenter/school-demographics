#Christopher Hu - Summer 2023 - ch7dm@virginia.edu
library(leaflet)
library(leaflegend)
library(dplyr)
library(htmltools)
library(readxl)

#Import data set: all values are percentages unless indicated by count
schoolmapdata2023 <- read_excel("fall 2023 updates/schoolmapdata_23.xlsx")
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

#Let's make a map

#Setting the colors 
districtpalette <- colorFactor(palette= c("#b45248","#50a9a6","#cab2d6","#2f7e9f","#004600","#360021","#002671", "#70B13D", "#E09056", "#0000CA"),
                               domain = c("Albemarle County", "Augusta County", "Buckingham County", "Charlottesville City", "Fluvanna County", "Greene County","Louisa County", "Nelson County", "Staunton City", "Waynesboro City"))

map <- leaflet(schoolmapdata2023) %>% addProviderTiles(providers$CartoDB) %>%
  setView(lng = -78.507, lat = 38.0322, zoom = 10) %>%
  addCircleMarkers (lng = ~schoolmapdata2023$Longitude, lat = ~schoolmapdata2023$Latitude,
                    radius = ~schoolmapdata2023$Total_Count*0.009, color = ~districtpalette(schoolmapdata2023$District),
                    label = paste("<strong>School:</strong>", schoolmapdata2023$School, "&nbsp;&nbsp;&nbsp;&nbsp;[", schoolmapdata2023$District, "]", 
                                  "<br><strong>Total</strong> =", schoolmapdata2023$Total_Count, "students","&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[", schoolmapdata2023$D_Total_Count, "students ]",
                                  "<br><strong>White</strong> =", sprintf("%.1f", schoolmapdata2023$White), "% &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[", sprintf("%.1f", schoolmapdata2023$D_White), "% ]",
                                  "<br><strong>Black</strong> =", sprintf("%.1f", schoolmapdata2023$Black),"% &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[", sprintf("%.1f", schoolmapdata2023$D_Black), "% ]",
                                  "<br><strong>Hispanic</strong> =", sprintf("%.1f", schoolmapdata2023$Hispanic),"% &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[", sprintf("%.1f", schoolmapdata2023$D_Hispanic), "% ]",
                                  "<br><strong>Asian</strong> =", sprintf("%.1f", schoolmapdata2023$Asian),"% &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[", sprintf("%.1f", schoolmapdata2023$D_Asian), "% ]",
                                  "<br><strong>Multiracial</strong> =", sprintf("%.1f", schoolmapdata2023$Multiracial),"% &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[", sprintf("%.1f", schoolmapdata2023$D_Multiracial), "% ]",
                                  "<br><strong>Econ. Disadvantaged</strong> =", sprintf("%.1f", schoolmapdata2023$Disadvantaged),"% &nbsp;&nbsp;&nbsp;&nbsp;[", sprintf("%.1f", schoolmapdata2023$D_Disadvantaged), "% ]",
                                  "<br><strong>English Learners</strong> =", sprintf("%.1f", schoolmapdata2023$EL), "% &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;[", sprintf("%.1f", schoolmapdata2023$D_EL), "% ]")
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
