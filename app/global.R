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
if (!require("sf")) {
  install.packages("sf")
  library(sf)
}
if (!require("tmap")) {
  install.packages("tmap")
  library(tmap)
}

data_source = 'remote'

#### Deactivate googlesheets authentication 
googlesheets4::gs4_deauth()

pick_borough <- function(x){
  ### identify borough crime took place in using patrols
  
  if(x=='PATROL BORO BRONX') {
    return ("BRONX")
  }
  else if (x=='PATROL BORO BKLYN SOUTH' || x=='PATROL BORO BKLYN NORTH'){
    return ("BROOKLYN")
  }
  else if (x=='PATROL BORO MAN NORTH' || x=='PATROL BORO MAN SOUTH'){
    return ("MANHATTAN")
  }
  else if (x=='PATROL BORO QUEENS NORTH' || x=='PATROL BORO QUEENS SOUTH'){
    return ("QUEENS")
  }
  else if (x=='PATROL BORO STATEN ISLAND'){
    return ("STATEN ISLAND")
  }
  else{
    return(NULL)
  } 
}

## Import covid cases dataset
if (data_source=='remote')
{
  covid_19_link <- 'https://docs.google.com/spreadsheets/d/1G2MuIPZYeROfXWy445tZRkpAzsyObA9RCp1jVgNP3mg/edit?usp=sharing'
  covid_19 <- read_sheet(covid_19_link)
}else if (data_source=='local')
{
  dir_path = '../data/'
  file_name ='COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv'
  file_path = paste(dir_path, file_name, sep="")
  covid_19 = read.csv(file_path)
}else
{
  stop("Select Data Source: local or Remote")
}
covid_19$DATE_OF_INTEREST <- as.Date(covid_19$DATE_OF_INTEREST, format = "%m/%d/%Y")

## Import Hate Crime dataset

if (data_source=='remote')
{
  hate_crime_link <- 'https://docs.google.com/spreadsheets/d/1KRNGjUtWfB3wSaQRoxmT9BTia_aO8NLquBG1BSG-mTg/edit?usp=sharing'
  Hate_Crimes <- read_sheet(hate_crime_link)
}else if (data_source=='local')
{
  dir_path = '../data/'
  hate_crime_file_name ='NYPD_Hate_Crimes.csv'
  hate_crime_file_path = paste(dir_path, hate_crime_file_name, sep="")
  Hate_Crimes <- read.csv(hate_crime_file_path, check.names=FALSE)
}else
{
  stop("Select Data Source: local or Remote")
}

Hate_Crimes$`Record Create Date` <- as.Date(Hate_Crimes$`Record Create Date`, format = "%m/%d/%Y")
Hate_Crimes["Borough Name"] = apply(Hate_Crimes["Patrol Borough Name"],1, pick_borough)
names(Hate_Crimes)[names(Hate_Crimes) == 'Borough Name'] <- 'boro_name'
hc_pre_covid = Hate_Crimes%>%filter(`Record Create Date` <= "2020-02-02") # pre first wave
hc_since_covid = Hate_Crimes%>%filter(`Record Create Date` > "2020-02-02") # since first wave

# create pivot table of hate crime dataframe
hc_b <- Hate_Crimes %>%
  select(`Record Create Date`, `Bias Motive Description`, `Full Complaint ID`) %>%
  group_by(`Record Create Date`, `Bias Motive Description`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))

### Hate crime counts by borough
# All time
hc_all_time_summarized = Hate_Crimes %>%
  select("boro_name", "Full Complaint ID") %>%
  group_by(`boro_name`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))
# pre first wave
hc_pre_covid_summarized = hc_pre_covid %>%
  select("boro_name", "Full Complaint ID") %>%
  group_by(`boro_name`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))
# post first wave
hc_since_covid_summarized = hc_since_covid %>%
  select("boro_name", "Full Complaint ID") %>%
  group_by(`boro_name`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))

dir_path = '../data/Borough Boundaries/'
gis_boundaries = 'geo_export_1866a9a8-81ce-4f8a-ba22-a52396bd4885.shp'
file_path = paste(dir_path, gis_boundaries, sep="")
aoi_boundary_NYC <- st_read(file_path)
aoi_boundary_NYC$boro_name = toupper(aoi_boundary_NYC$boro_name)

# merge hate crime df and gis df

hc_gis = merge(x = aoi_boundary_NYC, y = hc_all_time_summarized, by = "boro_name", all.x = TRUE)
hc_pre_covid_gis = merge(x = aoi_boundary_NYC, y = hc_pre_covid_summarized, by = "boro_name", all.x = TRUE)
hc_since_covid_gis = merge(x = aoi_boundary_NYC, y = hc_since_covid_summarized, by = "boro_name", all.x = TRUE)


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




