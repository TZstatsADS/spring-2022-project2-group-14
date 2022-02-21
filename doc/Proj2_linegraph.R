library("tidyverse")
library("leaflet")
library("readr")
library("ggplot2")
library("dplyr")
library("plotly")
library("gtable")
library("lubridate")

# dataset
Hate_Crimes <- read_csv("/Users/zengshanyue/Desktop/5243/Project/Project2/dataset/NYPD_Hate_Crimes.csv")
COVID_Daily_Cases <- read_csv("/Users/zengshanyue/Desktop/5243/Project/Project2/dataset/COVID-19_Daily_Counts_of_Cases__Hospitalizations__and_Deaths.csv")

COVID_Daily_Cases$DATE_OF_INTEREST <- as.Date(COVID_Daily_Cases$DATE_OF_INTEREST, format = "%m/%d/%Y")

COVID_Whole_Cases <- COVID_Daily_Cases %>%
  select(DATE_OF_INTEREST, CASE_COUNT, HOSPITALIZED_COUNT, DEATH_COUNT, CASE_COUNT_7DAY_AVG) %>%
  filter(DATE_OF_INTEREST >= "2020-02-29" & DATE_OF_INTEREST <= "2021-12-29")

COVID_Cases_Month <- COVID_Whole_Cases %>%
  mutate(Month = paste(year(DATE_OF_INTEREST),sprintf("%02d",month(DATE_OF_INTEREST)),sep="-")) %>%
  group_by(Month) %>%
  summarise(month_case = sum(CASE_COUNT))

Hate_Crimes$`Record Create Date` <- as.Date(Hate_Crimes$`Record Create Date`, format = "%m/%d/%Y")

Hate_Crimes_Cases <- Hate_Crimes %>%
  select(`Full Complaint ID`, `Record Create Date`, `Patrol Borough Name`, `County`, 
         `Law Code Category Description`,`Bias Motive Description`, `Offense Category`) %>%
  filter(`Record Create Date` >= "2020-02-29" & `Record Create Date` <= "2021-12-29")

Hate_Crimes_Cases_Day <- Hate_Crimes_Cases %>%
  select(`Full Complaint ID`, `Record Create Date`) %>%
  group_by(`Record Create Date`) %>%
  summarise(count = n_distinct(`Full Complaint ID`))

Hate_Crimes_Month <- Hate_Crimes %>%
  select(`Complaint Year Number`,`Month Number`,`Full Complaint ID`) %>%
  mutate(Month = paste(`Complaint Year Number`,`Month Number`),sep="-") %>%
  group_by(Month) %>%
  summarise(Case = n_distinct(`Full Complaint ID`))

### Interactive Plot(Separate)
# Whole Crime data cases by Month

data = Hate_Crimes %>% 
  select(`Full Complaint ID`, `Record Create Date`)  %>%
  count(date = format(as.Date(`Record Create Date`, format = "%m/%d/%Y"), "%Y/%m")) %>% 
  mutate(date = paste(date,"/01", sep = ""))  

  h1 <- data %>% 
    ggplot(aes(as.Date(date), n)) +
    geom_line() +
    geom_point() +
    xlab("Date") +
    ylab("Cases") +
    labs(title="Crime Cases By Month") +
    theme(plot.title=element_text(size=20, face="bold", hjust=0.5, lineheight=1.2)) +
    theme_bw()
  
  mytext <- paste("Date: ", data$date, "\n", 
                  "Number of Crime Cases: ", data$n,  sep="")    
  pp <- plotly_build(h1)   
  style(pp, text=mytext, hoverinfo = "text")

# Crime chart by Bias Motive Description
# ANTI-JEWISH
  
  data = Hate_Crimes[Hate_Crimes$`Bias Motive Description` == "ANTI-JEWISH", ] %>% 
    select(`Full Complaint ID`, `Record Create Date`)  %>%
    count(date = format(as.Date(`Record Create Date`, format = "%m/%d/%Y"), "%Y/%m")) %>% 
    mutate(date = paste(date,"/01", sep = ""))  
  
  h1 <- data %>% 
    ggplot(aes(as.Date(date), n)) +
    geom_line() +
    geom_point() +
    xlab("Date") +
    ylab("Cases") +
    labs(title="Crime Cases By Month") +
    theme(plot.title=element_text(size=20, face="bold", hjust=0.5, lineheight=1.2)) +
    theme_bw()
  
  mytext <- paste("Date: ", data$date, "\n", 
                  "Number of Crime Cases: ", data$n,  sep="")    
  pp <- plotly_build(h1)   
  style(pp, text=mytext, hoverinfo = "text")
  
# ANTI-ASIAN
  
  data = Hate_Crimes[Hate_Crimes$`Bias Motive Description` == "ANTI-ASIAN", ] %>% 
    select(`Full Complaint ID`, `Record Create Date`)  %>%
    count(date = format(as.Date(`Record Create Date`, format = "%m/%d/%Y"), "%Y/%m")) %>% 
    mutate(date = paste(date,"/01", sep = ""))  
  
  h1 <- data %>% 
    ggplot(aes(as.Date(date), n)) +
    geom_line() +
    geom_point() +
    xlab("Date") +
    ylab("Cases") +
    labs(title="Crime Cases By Month") +
    theme(plot.title=element_text(size=20, face="bold", hjust=0.5, lineheight=1.2)) +
    theme_bw()
  
  mytext <- paste("Date: ", data$date, "\n", 
                  "Number of Crime Cases: ", data$n,  sep="")    
  pp <- plotly_build(h1)   
  style(pp, text=mytext, hoverinfo = "text")

# ANTI-BLACK
  data = Hate_Crimes[Hate_Crimes$`Bias Motive Description` == "ANTI-BLACK", ] %>% 
    select(`Full Complaint ID`, `Record Create Date`)  %>%
    count(date = format(as.Date(`Record Create Date`, format = "%m/%d/%Y"), "%Y/%m")) %>% 
    mutate(date = paste(date,"/01", sep = ""))  
  
  h1 <- data %>% 
    ggplot(aes(as.Date(date), n)) +
    geom_line() +
    geom_point() +
    xlab("Date") +
    ylab("Cases") +
    labs(title="Crime Cases By Month") +
    theme(plot.title=element_text(size=20, face="bold", hjust=0.5, lineheight=1.2)) +
    theme_bw()
  
  mytext <- paste("Date: ", data$date, "\n", 
                  "Number of Crime Cases: ", data$n,  sep="")    
  pp <- plotly_build(h1)   
  style(pp, text=mytext, hoverinfo = "text")
  
# Crime cases by month
  
  h1 <- Hate_Crimes %>%
    select(`Record Create Date`, `Full Complaint ID`) %>%
    group_by(`Record Create Date`) %>%
    summarise(count = length(`Full Complaint ID`)) %>%
    ggplot(aes(`Record Create Date`, count)) +
    geom_line(color="#69b3a2") +
    xlab("Date") +
    ylab("Hate Crime Cases Count") +
    theme_bw()
  
  ggplotly(h1)
  
# COVID Cases by month
  
  COVID_Cases_Month <- COVID_Daily_Cases %>%
    select(CASE_COUNT, DATE_OF_INTEREST) %>%
    mutate(date = format(as.Date(DATE_OF_INTEREST, format = "%m/%d/%Y"), "%Y/%m")) %>% 
    mutate(date = paste(date,"/01", sep = "")) %>%
    group_by(date) %>%
    summarise(n = sum(CASE_COUNT))
  
  c1 <- COVID_Cases_Month %>%
    ggplot(aes(as.Date(date), n)) +
    geom_line() +
    geom_point() +
    xlab("Date") +
    ylab("Cases") +
    labs(title="COVID Cases By Month") +
    theme(plot.title=element_text(size=20, face="bold", hjust=0.5, lineheight=1.2)) +
    theme_bw()
  
  mytext <- paste("Date: ", data$date, "\n", 
                  "Number of COVID Cases: ", COVID_Cases_Month$n,  sep="")    
  pp <- plotly_build(c1)   
  style(pp, text=mytext, hoverinfo = "text")

# COVID Cases by day
  
  c1 <- COVID_Daily_Cases %>%
    ggplot(aes(DATE_OF_INTEREST, CASE_COUNT)) +
    geom_line(color="#69b3a2") +
    xlab("Date") +
    ylab("COVID Cases Count") +
    theme_bw()
  
  mytext <- paste("Date: ", COVID_Daily_Cases$DATE_OF_INTEREST, "\n", 
                  "Number of COVID Cases: ", COVID_Daily_Cases$CASE_COUNT,  sep="")    
  pp <- plotly_build(c1)   
  style(pp, text=mytext, hoverinfo = "text")
  
# Not interactive plot
  
  # linechart function
  
  linechart <- function(data1, data2){
    
    p <- ggplot(data1,aes(x=DATE_OF_INTEREST,y=CASE_COUNT))+
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
  
# Use function
  hc_b <- Hate_Crimes %>%
    select(`Record Create Date`, `Bias Motive Description`, `Full Complaint ID`) %>%
    group_by(`Record Create Date`, `Bias Motive Description`) %>%
    summarise(count = length(`Full Complaint ID`)) %>%
    arrange(desc(count))

  linechart(COVID_Daily_Cases, subset(hc_b, `Bias Motive Description` == "ANTI-MALE HOMOSEXUAL (GAY)"))

  # Whole data
  
  ggplot() + 
    geom_line(data = COVID_Whole_Cases, aes(x = DATE_OF_INTEREST, y = CASE_COUNT,color="COVID")) + 
    geom_line(data = Hate_Crimes_Cases_Day, aes(x=`Record Create Date`, y=(count-1)*5000, color="Crime")) +
    scale_y_continuous(name = "COVID Cases (per day)", sec.axis = sec_axis(~./5000+1, name = "Crime Cases (per day)")) +
    xlab("Date") +
    theme(axis.title.y = element_text(color = "red", size = 12),
          axis.title.y.right = element_text(color = "#39c3c2", size = 12)) +
    ggtitle("COVID Cases vs Crime Cases") 
  
  # Combination function
  combine <- function(data1,category){
    data2 <- Hate_Crimes[Hate_Crimes$`Bias Motive Description` == category, ] %>%
      select(`Full Complaint ID`, `Record Create Date`) %>%
      group_by(`Record Create Date`) %>%
      summarise(count = n_distinct(`Full Complaint ID`))
    
    ggplot() + 
      geom_line(data = data1, aes(x = DATE_OF_INTEREST, y = CASE_COUNT, color="COVID")) + 
      geom_line(data = data2, aes(x=`Record Create Date`, y=(count-1)*10000, color="Crime")) +
      scale_y_continuous(name = "COVID Cases (per day)", sec.axis = sec_axis(~./10000+1, name = "Crime Cases")) +
      xlab("Date") +
      theme(axis.title.y = element_text(color = "red", size = 12),
            axis.title.y.right = element_text(color = "#39c3c2", size = 12)) +
      ggtitle("COVID Cases vs Crime Cases")
  }
  
  combine(COVID_Whole_Cases,"ANTI-ASIAN")
  combine(COVID_Whole_Cases,"ANTI-MALE HOMOSEXUAL (GAY)")
  combine(COVID_Whole_Cases,"ANTI-JEWISH")
  combine(COVID_Whole_Cases,"ANTI-WHITE")
  combine(COVID_Whole_Cases,"ANTI-BLACK")
  