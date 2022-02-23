##### Install Required Packages

if (!require("ggplot2")) {
  install.packages("ggplot2")
  library(ggplot2)
}
if (!require("repr")) {
  install.packages("repr")
  library(repr)
}
if (!require("vroom")) {
  install.packages("vroom")
  library(vroom)
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
if (!require("RcppRoll")) {
  install.packages("RcppRoll")
  library(RcppRoll)
}
if (!require("tigris")) {
  install.packages("tigris")
  library(tigris)
}
if (!require("sp")) {
  install.packages("sp")
  library(sp)
}
if (!require("ggmap")) {
  install.packages("ggmap")
  library(ggmap)
}
if (!require("maptools")) {
  install.packages("maptools")
  library(maptools)
}
if (!require("broom")) {
  install.packages("broom")
  library(broom)
}
if (!require("httr")) {
  install.packages("httr")
  library(httr)
}
if (!require("rgdal")) {
  install.packages("rgdal")
  library(rgdal)
}
if (!require("ggthemes")) {
  install.packages("ggthemes")
  library(ggthemes)
}

if (!require("htmlwidgets")) {
  install.packages("htmlwidgets")
  library(htmlwidgets)
}


if (!require("tidyr")) {
  install.packages("tidyr")
  library(tidyr)
}

# Select data source
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

COVID_Whole_Cases <- covid_19 %>%
  select(DATE_OF_INTEREST, CASE_COUNT, HOSPITALIZED_COUNT, DEATH_COUNT, CASE_COUNT_7DAY_AVG) %>%
  filter(DATE_OF_INTEREST >= "2020-02-29" & DATE_OF_INTEREST <= "2021-12-29")

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

# Import NYC adminstrative boundaries
dir_path = 'www/Borough Boundaries/'
gis_boundaries = 'geo_export_1866a9a8-81ce-4f8a-ba22-a52396bd4885.shp'
file_path = paste(dir_path, gis_boundaries, sep="")
aoi_boundary_NYC <- st_read(file_path)
aoi_boundary_NYC$boro_name = toupper(aoi_boundary_NYC$boro_name)

## Import domestic violence dataset

if (data_source=='remote')
{
  domestic_link <- 'https://docs.google.com/spreadsheets/d/1jAHX4jW4H6LxqCZCtpfJKqqu-oCJe12S8c3JNJGTYqc/edit?usp=sharing'
  domestic_V <- read_sheet(domestic_link)
}else if (data_source=='local')
{
  dir_path = '../data/'
  domestic_file ='ENDGBV_Social_Media_Outreach__Paid_Advertising__and_the_NYC_HOPE_Resource_Directory_during_COVID-19.csv'
  domestic_file_path = paste(dir_path, domestic_file, sep="")
  domestic_V <- read.csv(domestic_file_path, check.names=FALSE)
}else
{
  stop("Select Data Source: local or Remote")
}

domestic_V$Date =  as.Date(domestic_V$Date, format = "%m/%d/%Y")
domestic_V$d7_rollavg = roll_mean(domestic_V$Visits, n = 7, align = "right", fill = NA)

## Import crime victims dataset

if (data_source=='remote')
{
  
  vic_link <- 'https://docs.google.com/spreadsheets/d/1ZKc6iCPhGfc2mSgNRRCwnWZQurR5VMqEBdhXOgrdK7Q/edit?usp=sharing'
  crime_vic_M <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1Cseydumt4CCHtdTUS4Ta8_Tuca9dvN32f8tQIgN26zo/edit?usp=sharing'
  crime_vic_F <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1lyPYz76HSF_pMYZWNxNTEG_hxzn8ScpVOkP3hcy8xo4/edit?usp=sharing'
  crime_vic_D <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1hBNf8GKoowmMCi5Lq3Dt3psZQ3CPyw6mPIDCjlX1lak/edit?usp=sharing'
  crime_vic_E <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1vqR3SrZJIjd973T940BD-ijtNySDFwpsbAuCM6m5fPw/edit?usp=sharing'
  crime_vic_18 <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1SnBfcvErFUwvyj6DY0G9NDveAzOqW9OsGqFzF_Eqha0/edit?usp=sharing'
  crime_vic_24 <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1IG8oJY3ezJjs4pQU144x6Av1OBdlZ9qpKeHtQCtXMkk/edit?usp=sharing'
  crime_vic_44 <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1bAcKrdISknm2e9_ZvhcAWsrHEYPmeGj0eWMPuYYafXY/edit?usp=sharing'
  crime_vic_64 <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1abtH6_TSwTWanVbAmMM6yK7roHS5gqQap1hCiso_iYM/edit?usp=sharing'
  crime_vic_65 <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1sMtaj-ymYqQc1zPcnStEn9Se0FOpS3-70ULvBC-p9MY/edit?usp=sharing'
  crime_vic_asian <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1OtI-Pk6UkHGTORdacVNcoWOiqlxwNkYeRT3_uzguEIs/edit?usp=sharing'
  crime_vic_black_his <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/14cndmavdWwLk6gZ6w6oFKL1gfPs9wVsXE30D2tLj8Pc/edit?usp=sharing'
  crime_vic_black <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1EKngLl45QzC2NYS7E5UQziuqZiSoefIQou5OiarSyHQ/edit?usp=sharing'
  crime_vic_native <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1lk8LF7IWys7nVKh9S3pcsuyJ8OU3BY9XsGd0RWpJrd4/edit?usp=sharing'
  crime_vic_white_his <- read_sheet(vic_link)
  vic_link <- 'https://docs.google.com/spreadsheets/d/1T4B2pMLBhcKP0GPwbvhFF8lWgKj1jhr74gs9zHl2rNQ/edit?usp=sharing'
  crime_vic_white <- read_sheet(vic_link)
  
}else if (data_source=='local')
{
  dir_path = '../output/crime_complaints/'
  vic_file ='VIC_SEX_M.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_M <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_SEX_F.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_F <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_SEX_D.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_D <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_SEX_E.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_E <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_AGE_GROUP_18.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_18 <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_AGE_GROUP_18-24.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_24 <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_AGE_GROUP_25-44.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_44 <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_AGE_GROUP_45-64.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_64 <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_AGE_GROUP_65+.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_65 <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_RACE_ASIAN_PACIFIC_ISLANDER.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_asian <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_RACE_BLACK HISPANIC.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_black_his <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_RACE_BLACK.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_black <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_RACE_native_american.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_native <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_RACE_WHITE HISPANIC.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_white_his <- read.csv(vic_file_path, check.names=FALSE)
  
  vic_file ='VIC_RACE_WHITE.csv'
  vic_file_path = paste(dir_path, vic_file, sep="")
  crime_vic_white <- read.csv(vic_file_path, check.names=FALSE)
  
}else
{
  stop("Select Data Source: local or Remote")
}
# '''
# if (data_source==)
# {
#  NYPDComplaint <- read_sheet(nypdComplaint_link)
# }else if (data_source==local)
# {
#   dir_path = ../output/crime_complaints/
#   NYPDComplaint_file_name =NYPDComplaint_Processed.csv
#   NYPDComplaint_file_path = paste(dir_path, NYPDComplaint_file_name, sep="")
#   NYPDComplaint <- read.csv(NYPDComplaint_file_path, check.names=FALSE)
# }else
# {
#   stop("Select Data Source: local or Remote")
# }'''

dir_path = 'www/'
NYPDComplaint_file_name ='NYPDComplaint_Processed.csv'
NYPDComplaint_file_path = paste(dir_path, NYPDComplaint_file_name, sep="")
NYPDComplaint <- read.csv(NYPDComplaint_file_path, check.names=FALSE)

# merge hate crime df and gis df
hc_gis = merge(x = aoi_boundary_NYC, y = hc_all_time_summarized, by = "boro_name", all.x = TRUE)
hc_pre_covid_gis = merge(x = aoi_boundary_NYC, y = hc_pre_covid_summarized, by = "boro_name", all.x = TRUE)
hc_since_covid_gis = merge(x = aoi_boundary_NYC, y = hc_since_covid_summarized, by = "boro_name", all.x = TRUE)

# Combination function 1
combine1 <- function(data1,category){
  data2 <- Hate_Crimes[Hate_Crimes$`Bias Motive Description` == category, ] %>%
    select(`Full Complaint ID`, `Record Create Date`) %>%
    group_by(`Record Create Date`) %>%
    summarise(count = n_distinct(`Full Complaint ID`))
  
  ggplot() + 
    geom_line(data = data1, aes(x = DATE_OF_INTEREST, y = CASE_COUNT_7DAY_AVG, color="COVID")) + 
    geom_line(data = data2, aes(x=`Record Create Date`, y=(count-1)*10000, color="Crime")) +
    scale_y_continuous(name = "COVID Cases 7day Average", sec.axis = sec_axis(~./10000, name = "Crime Cases")) +
    xlab("Date") +
    theme(axis.title.y = element_text(color = "red", size = 12),
          axis.title.y.right = element_text(color = "#39c3c2", size = 12)) +
    ggtitle("COVID Cases vs Crime Cases")
}

# Combination function 2
combine2 <- function(data1,data2){
  ggplot() + 
    geom_line(data = data1, aes(x = DATE_OF_INTEREST, y = CASE_COUNT_7DAY_AVG, color="COVID")) + 
    geom_line(data = data2, aes(x=Date, y=(Visits)*2, color="Crime")) +
    scale_y_continuous(name = "COVID Cases 7day Average", sec.axis = sec_axis(~./2, name = "Crime Cases")) +
    xlab("Date") + 
    theme(axis.title.y = element_text(color = "red", size = 12),
          axis.title.y.right = element_text(color = "#39c3c2", size = 12)) +
    ggtitle("COVID Cases vs Crime Cases")
}

# Combination function 3
combine3 <- function(data1,data2){
  
  data2$CMPLNT_FR_DT =  as.Date(data2$CMPLNT_FR_DT, format = "%Y-%m-%d")
  data2$d7_rollavg = as.numeric(unlist(data2$d7_rollavg))
  
  ggplot() + 
    geom_line(data = data1, aes(x = DATE_OF_INTEREST, y = CASE_COUNT_7DAY_AVG, color="COVID")) + 
    geom_line(data = data2, aes(x=CMPLNT_FR_DT, y=(d7_rollavg)*10, color="Crime")) +
    scale_y_continuous(name = "COVID Cases 7day Average", sec.axis = sec_axis(~./10, name = "Crime Cases"),limits=c(0,7500)) +
    xlab("Date") + 
    theme(axis.title.y = element_text(color = "red", size = 12),
          axis.title.y.right = element_text(color = "#39c3c2", size = 12)) +
    ggtitle("COVID Cases vs Crime Cases")
}

combine4 <- function(data1){
  
  data_hate <- data1
  county <- data_hate$County
  lat <- NA
  lat <- ifelse(county=="BRONX", 40.844782, lat)
  lat <- ifelse(county=="RICHMOND", 40.5834379, lat)
  lat <- ifelse(county=="NEW YORK", 40.730610, lat)
  lat <- ifelse(county=="QUEENS", 40.742054, lat)
  lat <- ifelse(county=="KINGS", 40.819824, lat)
  long <- NA
  long <- ifelse(county=="BRONX", -73.864827, long)
  long <- ifelse(county=="RICHMOND", -74.1495875, long)
  long <- ifelse(county=="NEW YORK", -73.935242, long)
  long <- ifelse(county=="QUEENS", -73.769417, long)
  long <- ifelse(county=="KINGS", -73.735130, long)
  data_hate$lat <- lat
  data_hate$long <- long
  # Plot
  m <- leaflet(data_hate) %>% 
    addTiles() # add the background map 
  library(RColorBrewer)
  pal = colorFactor("Set1", domain = data_hate$County) # Grab a palette
  color_offsel1 = pal(data_hate$County)
  mclust <- m %>% addCircleMarkers(color = color_offsel1, 
                                   popup = content,
                                   clusterOptions = markerClusterOptions()) %>%
    addLegend(pal = pal, values = ~data_hate$County, title = "Counties")
  mclust
}

combine5 <- function(data1){
  
  register_google(key = "AIzaSyCVWbGYizBSs2nZktIsO-GvOM3eaWSaizg", write= TRUE)
  map_Manhattan_st <- get_map("New York",
                              zoom = 12,
                              source = "stamen",
                              maptype = "toner-background")
  
  ggmap(map_Manhattan_st)
  
  data1 <- data1
  colnames(data1)
  data2 <- data1[,c(1,2,3)]
  colnames(data2)
  data3 <- na.omit(data2)
  data3.df <- as.data.frame(data3)
  data3.df$RPT_DT <- paste(substr(data3.df$RPT_DT , 1, 3),substr(data3.df$RPT_DT , 7, 10))
  
  g <- ggmap(map_Manhattan_st) + theme_map()
  g + geom_point(data=data3.df, aes(x=Longitude,y=Latitude),
                 size=0.3, alpha=0.3, color="red")
  
  
  hc=g + stat_density2d(data = data3.df, geom = "polygon",
                        aes(x = Longitude, y = Latitude, fill=..level..,alpha=..level..)) + 
    scale_fill_gradient(low = "yellow", high = "red") + theme_map()
  
  hc
  
}


# Category choice selector

selector <- function(category){
  if (category=="VIC_AGE_GROUP"){
    return(
      c('<18','18-24','25-44','45-64','65+')
    )
  }
  if (category=="VIC_RACE"){
    return(
      c('BLACK','BLACK HISPANIC',
        'WHITE HISPANIC',
        'WHITE',
        'ASIAN / PACIFIC ISLANDER',
        'AMERICAN INDIAN/ALASKAN NATIVE'
      )
    )
  }
  if (category=="VIC_SEX"){
    return(
      c('M', 'F', 'E', 'D')
    )
  }
}


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
