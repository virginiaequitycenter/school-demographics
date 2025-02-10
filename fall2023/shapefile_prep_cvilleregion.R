##############################################################
# School Boundaries Shapefile Prep                    
# Authors: Asha Muralidharan, Chris Hu     
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

shape_hs <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_High.shp") 
shape_ms <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_Middle.shp")
shape_ps <- st_read(dsn="SABS_1516_SchoolLevels/SABS_1516_Primary.shp")

incl_lea <- c("5100090","5100300","5100540","5100780","5101380","5101710","5102280","5102580","5103690","5103930","5102820","5102370")

shape_hs <- shape_hs %>% subset(stAbbrev=="VA")
shape_hs <- shape_hs %>% subset(leaid %in% incl_lea)
shape_hs <- shape_hs %>% st_transform("+init=epsg:4326")

shape_ms <- shape_ms %>% subset(stAbbrev=="VA")
shape_ms <- shape_ms %>% subset(leaid %in% incl_lea)
shape_ms <- shape_ms %>% st_transform("+init=epsg:4326")

shape_ps <- shape_ps %>% subset(stAbbrev=="VA")
shape_ps <- shape_ps %>% subset(leaid %in% incl_lea)
shape_ps <- shape_ps %>% st_transform("+init=epsg:4326")

st_write(shape_hs, "School Boundary Files 201516/SABS_1516_VA_CvilleRegion_High.shp")
st_write(shape_ms, "School Boundary Files 201516/SABS_1516_VA_CvilleRegion_Middle.shp")
st_write(shape_ps, "School Boundary Files 201516/SABS_1516_VA_CvilleRegion_Primary.shp")


