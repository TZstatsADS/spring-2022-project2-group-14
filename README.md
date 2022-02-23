# Project 2: Shiny App Development

## Analyzing Crime Data in New York pre and post-Covid-19 
### Shiny App Link: https://rishav.shinyapps.io/Covid19_HateCrime/
Term: Spring 2022

+ Team #14
+ **Team Members**:
	+ Team Member 1: Ananya Iyer (ai2446)
	+ Team Member 2: Joel Mugyenyi (jm5352)
	+ Team Member 3: Lichun He (lh3094)
	+ Team Member 4: Rishav Agarwal (ra3141)
	+ Team Member 5: Shanyue Zeng (sz2896)

+ **Project summary**: Covid-19 changed the course of the world. If it helped a united side of the world come out, then it also showed the ugly face of hate crime that has plagued this earth. New York City had to endure this burn as well. This application hopes to bring to light how the rise and fall of COVID-19 affected the crime trends of the city, especially bringing emphasis to the Hate Crimes that were reported.

+ **Contribution statement**: 

	+ ***Joel Mugyenyi*** is the project Presenter. He searched for and obtained datasets team used for the project, carried out exploratory analysis on domestic violence, 	hate crime crime complaints, covid cases and motor accident datasets, suggested visualizations to show temporal and spatial changes in crime due to covid 19, created 		visualizations in python which another team member later converted to R, created sample shiny App template showing line charts and maps, hosted 6 of the 9 group 		meetings held by team (sent out calendars to remind members to attend meetings), figured out how to host and access data on googlesheets so it’s available to deployed 		app, assisted teammates in debugging of code, added domestic violence and crime victim sections to app, created and added heatmap showing hate crimes before and during 	covid era.
	+ ***Lichun He*** was focused on the Shiny app and mainly dealt with the line chart part. Lichun provided a Shiny app template as reference during the creation of Shiny 	 app. Based on the line chart code Shanyue provided and some self-learning knowledge about Shinydashboard, Lichun made some changes and improvements to make the plot 		user-friendly and adequate to be showing up in the shiny dashboard template provided by Joel. Lichun also created related tables that summarize the information from the 	 line chart and also made it interactively. Besides, Lichun scheduled a meeting with Shanyue and Ananya to provide some help and collaborate in data visualization with  	 Shanyue and in Shiny app development with Ananya. Lichun attended all the group meetings and followed the instructions discussed in the meeting.        
	+ ***Rishav Agarwal***  worked on exploring two datasets, namely the NYPD Hate Crime dataset and the NYPD Complaint Dataset. He was majorly involved in creating		spatial visualizations using two different mapping techniques (leaflet and cloud-based ggmap). He created a leaflet map focusing on the Hate Crimes across counties, by 	adding Latitude and Longitude to the dataset. Using the cloud based-ggmap, he visualized a haet map over Manhattan crime dataset. Besides the visualization, he also 		succesfully deployed the Shiny App after integrating his visualizations with the dashboard created by other team members. During the initial phase of the project, he 		also facilitated team meeting by creating the required groups. He also assisted the team in the documentation of the project. 
	+ ***Ananya Iyer*** worked on the creating the dashboard of the Shiny App, where she focised on integrating the visualizations created by others. She worked with Lichun 	 to create the shiny app template while also working with other team members to solve the bugs that the team came across. She also contributed in visualization aspect by 	  providing an analysis of the covid19 trends in New York City. 
	+ ***Shanyue Zeng*** mainly focused on data analytic and data visualization. She did data cleansing and preprocessing before analyzing the data. Besides, she plotted 	         the interactive linecharts for both covid and crime dataset, as well as the comparision linechart for combined datasets using multiple methods for better visualization. 	  She also helped in data visualization about the map, and collaborated with Lichun to deal with some problems and improve Shiny dashboard visualization.    
	
	+ All team members collaborated over Zoom meetings to carry out tasks and completed their assigned tasks diligently. All team members approve our work presented in this 	 GitHub repository including this contributions statement. 

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

