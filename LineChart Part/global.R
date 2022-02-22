##### Install and Load Required Packages
packages.used=c("shiny", "ggplot2", "repr", "dplyr", "plotly", 
                "gtable", "leaflet", "readr", "grid", "googlesheets4",
                "tigris", "sp", "ggmap", "maptools", "broom",
                "httr", "rgdal", "RColorBrewer", "knitr", "lubridate")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE,
                   repos='http://cran.us.r-project.org')}
library(shiny)
library(ggplot2)
library(repr)
library(dplyr)
library(plotly)
library(gtable)
library(leaflet)
library(readr)
library(grid)
library(googlesheets4)
library(tigris)
library(sp)
library(ggmap)
library(maptools)
library(lubridate)
library(broom)
library(httr)
library(rgdal)
library(RColorBrewer)
library(knitr)

data_source = 'local'

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
  dir_path = '/Users/helichun/Desktop/'
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
  dir_path = '/Users/helichun/Desktop/'
  hate_crime_file_name ='NYPD_Hate_Crimes.csv'
  hate_crime_file_path = paste(dir_path, hate_crime_file_name, sep="")
  Hate_Crimes <- read.csv(hate_crime_file_path, check.names=FALSE)
}else
{
  stop("Select Data Source: local or Remote")
}

Hate_Crimes$`Record Create Date` <- as.Date(Hate_Crimes$`Record Create Date`, format = "%m/%d/%Y")
Hate_Crimes["Borough Name"] = apply(Hate_Crimes["Patrol Borough Name"], 1, pick_borough)
names(Hate_Crimes)[names(Hate_Crimes) == 'Borough Name'] <- 'boro_name'
hc_pre_covid = Hate_Crimes%>%filter(`Record Create Date` <= "2020-02-02") # pre first wave
hc_since_covid = Hate_Crimes%>%filter(`Record Create Date` > "2020-02-02") # since first wave


### Hate crime counts by borough
# All time
hc_all_time_summarized = Hate_Crimes %>%
  dplyr::select("boro_name", "Full Complaint ID") %>%
  group_by(`boro_name`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))
# pre first wave
hc_pre_covid_summarized = hc_pre_covid %>%
  dplyr::select("boro_name", "Full Complaint ID") %>%
  group_by(`boro_name`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))
# post first wave
hc_since_covid_summarized = hc_since_covid %>%
  dplyr::select("boro_name", "Full Complaint ID") %>%
  group_by(`boro_name`) %>%
  summarise(count = length(`Full Complaint ID`)) %>%
  arrange(desc(count))

dir_path = '/Users/helichun/Desktop/shiny v4/data/Borough Boundaries/'
gis_boundaries = 'geo_export_1866a9a8-81ce-4f8a-ba22-a52396bd4885.shp'
file_path = paste(dir_path, gis_boundaries, sep="")
aoi_boundary_NYC <- st_read(file_path)
aoi_boundary_NYC$boro_name = toupper(aoi_boundary_NYC$boro_name)

# merge hate crime df and gis df

hc_gis = merge(x = aoi_boundary_NYC, y = hc_all_time_summarized, by = 'boro_name', all.x = TRUE)
hc_pre_covid_gis = merge(x = aoi_boundary_NYC, y = hc_pre_covid_summarized, by = "boro_name", all.x = TRUE)
hc_since_covid_gis = merge(x = aoi_boundary_NYC, y = hc_since_covid_summarized, by = "boro_name", all.x = TRUE)

#-------------------------------------------------------------------------------
# Line chart 
#-------------------------------------------------------------------------------
plot_line <- function(selected_dateS, selected_dateE,
                      selected_county, selected_motive, selected_topic){
  
  Hate_Crimes_line <- Hate_Crimes %>%
    filter(`Record Create Date` >= as.Date(selected_dateS) & `Record Create Date` <= as.Date(selected_dateE))
  if (selected_county != "All"){
    Hate_Crimes_line <- Hate_Crimes_line %>% filter(`boro_name` == selected_county)}
  if (selected_motive != "All"){
    Hate_Crimes_line <- Hate_Crimes_line %>% filter(`Bias Motive Description` == selected_motive)}
  
  covid_19_line <- covid_19 %>%
    filter(DATE_OF_INTEREST >= as.Date(selected_dateS) & DATE_OF_INTEREST <= as.Date(selected_dateE))
  if (selected_county == "All"){
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST:all_death_count_7day_avg)}
  if (selected_county == "BRONX") {
    covid_19_line <- covid_19_line %>% 
      dplyr::select(DATE_OF_INTEREST, BX_CASE_COUNT:bx_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `BX_CASE_COUNT`)}
  if (selected_county == "QUEENS") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, QN_CASE_COUNT:qn_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `QN_CASE_COUNT`)}
  if (selected_county == "MANHATTAN") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, MN_CASE_COUNT:mn_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `MN_CASE_COUNT`)}
  if (selected_county == "STATEN ISLAND") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, SI_CASE_COUNT:si_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `SI_CASE_COUNT`)}
  if (selected_county == "BROOKLYN") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, BK_CASE_COUNT:bk_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `BK_CASE_COUNT`)}
  
  # Hate Crime
  data = Hate_Crimes_line %>% 
    dplyr::select(`Full Complaint ID`, `Record Create Date`)  %>%
    count(date = format(as.Date(`Record Create Date`, format = "%m/%d/%Y"), "%Y/%m")) %>% 
    mutate(date = paste(date,"/01", sep = ""))  
  
  h1 <- data %>% 
    ggplot(aes(as.Date(date), n)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = as.Date("2020-02-01"), col='red') + 
    xlab("Date") +
    ylab("Cases") +
    labs(title="Crime Cases in This Time Period") +
    theme(plot.title=element_text(size=20, face="bold", hjust=0.5, lineheight=1.2)) +
    theme_bw()
  plot1 <- plotly_build(h1)   
  
  # Covid19
  data2 <- covid_19_line %>%
    dplyr::select(CASE_COUNT, DATE_OF_INTEREST) %>%
    mutate(date = format(as.Date(DATE_OF_INTEREST, format = "%m/%d/%Y"), "%Y/%m")) %>% 
    mutate(date = paste(date,"/01", sep = "")) %>%
    group_by(date) %>%
    summarise(n = sum(CASE_COUNT))
  
  c1 <- data2 %>%
    ggplot(aes(as.Date(date), n)) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = as.Date("2020-02-01"), col='red') + 
    xlab("Date") +
    ylab("Cases") +
    labs(title="COVID Cases in This Time Period") +
    theme(plot.title=element_text(size=20, face="bold", hjust=0.5, lineheight=1.2)) +
    theme_bw()
  plot2 <- plotly_build(c1)   
  
  # linechart function
  linechart <- function(data1, data2){
    p <- ggplot(data1,aes(x=DATE_OF_INTEREST,y=CASE_COUNT))+
      geom_line(color="blue") +
      geom_vline(xintercept = as.Date("2020-02-01"), col='red') + 
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
  
  # Use function
  hc_b <- Hate_Crimes_line %>%
    dplyr::select(`Record Create Date`, `Bias Motive Description`, `Full Complaint ID`) %>%
    group_by(`Record Create Date`, `Bias Motive Description`) %>%
    summarise(count = length(`Full Complaint ID`)) %>%
    arrange(desc(count))
  
  if (selected_topic == "Both"){
    linechart(covid_19_line, hc_b)
  } else if (selected_topic == "Hate Crime"){
    h1
  } else {c1}
  
}

plot_line_table <- function(selected_dateS, selected_dateE,
                      selected_county, selected_motive, selected_topic){
  if (selected_motive != "All"){
    hc_pre_covid <- hc_pre_covid %>% filter(`Bias Motive Description` == selected_motive)
    hc_since_covid <- hc_since_covid %>% filter(`Bias Motive Description` == selected_motive)}
  
  tab1 <- hc_pre_covid %>%
    dplyr::select(`Bias Motive Description`, `Full Complaint ID`) %>%
    group_by(`Bias Motive Description`) %>%
    summarise(cases_before_covid = length(`Full Complaint ID`))
  tab2 <- hc_since_covid %>%
    dplyr::select(`Bias Motive Description`, `Full Complaint ID`) %>%
    group_by(`Bias Motive Description`) %>%
    summarise(cases_during_covid = length(`Full Complaint ID`))
  
  
  Hate_Crimes_line <- Hate_Crimes %>%
    filter(`Record Create Date` >= as.Date(selected_dateS) & `Record Create Date` <= as.Date(selected_dateE))
  if (selected_county != "All"){
    Hate_Crimes_line <- Hate_Crimes_line %>% filter(`boro_name` == selected_county)}
  if (selected_motive != "All"){
    Hate_Crimes_line <- Hate_Crimes_line %>% filter(`Bias Motive Description` == selected_motive)}
  
  covid_19_line <- covid_19 %>%
    filter(DATE_OF_INTEREST >= as.Date(selected_dateS) & DATE_OF_INTEREST <= as.Date(selected_dateE))
  if (selected_county == "All"){
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST:all_death_count_7day_avg)}
  if (selected_county == "BRONX") {
    covid_19_line <- covid_19_line %>% 
      dplyr::select(DATE_OF_INTEREST, BX_CASE_COUNT:bx_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `BX_CASE_COUNT`)}
  if (selected_county == "QUEENS") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, QN_CASE_COUNT:qn_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `QN_CASE_COUNT`)}
  if (selected_county == "MANHATTAN") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, MN_CASE_COUNT:mn_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `MN_CASE_COUNT`)}
  if (selected_county == "STATEN ISLAND") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, SI_CASE_COUNT:si_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `SI_CASE_COUNT`)}
  if (selected_county == "BROOKLYN") {
    covid_19_line <- covid_19_line %>% dplyr::select(DATE_OF_INTEREST, BK_CASE_COUNT:bk_all_death_count_7day_avg)%>%
      rename(CASE_COUNT = `BK_CASE_COUNT`)}

  tableHC <- Hate_Crimes_line%>%
    dplyr::select("boro_name", `Bias Motive Description`, "Full Complaint ID") %>%
    group_by(`boro_name`, `Bias Motive Description`) %>%
    summarise(count = length(`Full Complaint ID`)) %>%
    arrange(desc(count)) %>%
    rename(County = "boro_name",
           Motive = `Bias Motive Description`,
           "Hate Crime Cases during this time period" = count)
  
  tableCovid <- covid_19_line %>%
    dplyr::select(CASE_COUNT, DATE_OF_INTEREST) %>%
    mutate(date = format(as.Date(DATE_OF_INTEREST, format = "%m/%d/%Y"), "%Y/%m")) %>% 
    group_by(date) %>%
    summarise("Covid Cases during this time period" = sum(CASE_COUNT))
  
  if (selected_topic == "Both"){
    print(left_join(tab1, tab2))
  } else if (selected_topic == "Hate Crime"){
    tableHC
  } else {tableCovid}

}



#-------------------------------------------------------------------------------
# Map
#-------------------------------------------------------------------------------
