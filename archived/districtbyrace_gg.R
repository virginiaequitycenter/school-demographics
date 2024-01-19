#Christopher Hu - Summer 2023 - ch7dm@virginia.edu
#Load packages
library(ggplot2)
library(ggiraph)
library(dplyr)

#Import dataset - compiled from VDOE Build-a-Table
library(readxl)
districtbyrace_23 <- read_excel("districtbyrace_23.xlsx")
View(districtbyrace_23)  

#Calculating Percentages 
race <- districtbyrace_23 %>%
  group_by(District) %>%
  mutate(Percentage = Count/sum(Count)*100)

race$Percentage <- round(race$Percentage, 1)

#Removing Indigenous: too small of counts to appear on chart
race <- race[- grep("Indigenous", race$Race),]

#Customizing Colors
custom_colors <- c("#e27462","#2f7e9f","#50a9a6","#782c6a","#93aede")

#Reordering the X & Y 
race$District <- factor(race$District, levels = c("Augusta", "Buckingham", "Staunton", "Waynesboro", "Nelson", "Louisa", "Greene", "Fluvanna", "Charlottesville", "Albemarle"))
race$Race <- factor(race$Race, levels = c("Multiracial", "Asian", "Hispanic", "Black", "White"))

race <- race %>% mutate(tooltip_text = paste0(Percentage, "% ", Race, " (", Count, " students)"))

####ggplot & ggiraph 
gg <- ggplot(race, aes(fill = Race, y = Count, x = District, tooltip = tooltip_text)) +
  geom_bar_interactive(position = "fill", stat = "identity", width = 0.7, color = "black") +
  scale_fill_manual(values = custom_colors) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Racial Demographics of Local School Districts, 2023") +
  labs(caption = "Multiracial indicates non-Hispanic & two or more races; Indigenous students not included.") +
  scale_y_continuous(labels = scales::percent) +
  guides(fill = guide_legend(reverse = TRUE)) + 
  theme(plot.caption = element_text(size = 2), axis.title.y = element_blank(), axis.title.x = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(fill = NULL) + 
  theme(plot.margin = margin(1,1,1.5,1.5, "cm")) + 
  theme(plot.title = element_text(face = "bold", size = 12))
    
tooltip_css <- "background-color:#d3d3d3; font-family: Arial; color:black; border: 1px solid black; font-size: 14px; padding: 8px"
y <- girafe(ggobj = gg,
                options = list(
                  opts_sizing(width = 1.0), 
                  opts_tooltip(css = tooltip_css)))

print(y)

