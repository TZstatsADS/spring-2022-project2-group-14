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
if (!require("RcppRoll")) {
  install.packages("RcppRoll")
  library(RcppRoll)
}

# Select data source
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
  
  vic_file ='VIC_AGE_GROUP_<18.csv'
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
