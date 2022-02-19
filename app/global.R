##### Install Required Packages

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("repr")) {
  install.packages("repr")
  library(repr)
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)
}
if (!require("plotly")) {
  install.packages("plotly")
  library(plotly)
}
if (!require("gtable")) {
  install.packages("gtable")
  library(gtable)
}
if (!require("leaflet")) {
  install.packages("leaflet")
  library(leaflet)
}
if (!require("readr")) {
  install.packages("readr")
  library(readr)
}
if (!require("grid")) {
  install.packages("grid")
  library(grid)
}
if (!require("googlesheets4")) {
  install.packages("googlesheets4")
  library(googlesheets4)
}

#### Deactivate googlesheets authentication 
googlesheets4::gs4_deauth()

## Import covid cases dataset
covid_19_link <- 'https://docs.google.com/spreadsheets/d/1G2MuIPZYeROfXWy445tZRkpAzsyObA9RCp1jVgNP3mg/edit?usp=sharing'
covid_19 <- read_sheet(covid_19_link)
covid_19$DATE_OF_INTEREST <- as.Date(covid_19$DATE_OF_INTEREST, format = "%m/%d/%Y")

## Import Hate Crime dataset
hate_crime_link <- 'https://docs.google.com/spreadsheets/d/1KRNGjUtWfB3wSaQRoxmT9BTia_aO8NLquBG1BSG-mTg/edit?usp=sharing'
Hate_Crimes <- read_sheet(hate_crime_link)
Hate_Crimes$`Record Create Date` <- as.Date(Hate_Crimes$`Record Create Date`, format = "%m/%d/%Y")

# create pivot table of hate crime dataframe
hc_b <- Hate_Crimes %>%
  select(`Record Create Date`, `Bias Motive Description`, `Full Complaint ID`) %>%
  group_by(`Record Create Date`, `Bias Motive Description`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))


# Linechart of hate crime and covid cases
linechart <- function(data1, data2){
  
  p <- ggplot(data1,aes(x=DATE_OF_INTEREST,y=CASE_COUNT_7DAY_AVG))+
    geom_line(color="blue") +
    xlab("Date") +
    ylab("COVID Cases Count") +
    theme_bw()
  
  p1 <- p +
    ylim(0,50000)+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          panel.background=element_blank(),
          legend.position="top")
  
  p2_1 <- ggplot(data2, aes(x = `Record Create Date`, y = count)) + 
    geom_line(color="#69b3a2")
  
  p2 <- p2_1+
    ylim(1,max(data2$count))+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_blank(),
          axis.title.x =element_blank(),
          axis.title.y =element_blank())%+replace% 
    theme(panel.background = element_rect(fill = NA))
  
  # gtable
  g1 <- ggplot_gtable(ggplot_build(p1))
  g2 <- ggplot_gtable(ggplot_build(p2))
  
  # overlap
  pp <- c(subset(g1$layout, name == "panel", se = t:r))
  g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], 
                       pp$t,pp$l, pp$b, pp$l)
  grid.newpage()
  
  
  #axis tweaks
  ia <- which(g2$layout$name == "axis-l")
  ga <- g2$grobs[[ia]]
  ax <- ga$children[[2]]
  ax$widths <- rev(ax$widths)
  ax$grobs <- rev(ax$grobs)
  ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths))
  g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)
  
  grid.newpage() 
  grid.draw(g)
  
}
