# Project 2: Shiny App Development

### [Project Description](doc/project2_desc.md)

![screenshot](doc/figs/map.jpg)

In this second project of GR5243 Applied Data Science, we develop a *Exploratory Data Analysis and Visualization* shiny app on the work of a **NYC government agency/program** of your choice using NYC open data released on the [NYC Open Data By Agency](https://opendata.cityofnewyork.us/data/) website. In particular, many agencies have adjusted their work or rolled out new programs due to COVID, your app should provide ways for a user to explore quantiative measures of how covid has impacted daily life in NYC from different prospectives. See [Project 2 Description](doc/project2_desc.md) for more details.  

The **learning goals** for this project is:

- business intelligence for data science
- data cleaning
- data visualization
- systems development/design life cycle
- shiny app/shiny server

*The above general statement about project 2 can be removed once you are finished with your project. It is optional.

## Analyzing Crime Data in New York pre and post-Covid19 
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

	+ ***Joel Mugyenyi*** is the project Presenter. He searched for and obtained datasets team used for the project, carried out exploratory analysis on domestic violence, 	hate crime crime complaints, covid cases and motor accident datasets, suggested visualizations to show temporal and spatial changes in crime due to covid 19, created 		visualizations in python which another team member later converted to R, created sample shiny App template showing line charts and maps, hosted 6 of the 9 group 		meetings held by team(sent out calendars to remind members to attend meetings), figured out how to host and access data on googlesheets so it’s available to deployed 		app, assisted teammates in debugging of code, added domestic violence and crime victim sections to app, added heatmap showing hate crimes before and during covid era.
	+ ***Rishav Agarwal*** initiated the project conversation and organized the deliverables within the team. He worked on exploring two datasets, namely the NYPD Hate 		Crime dataset and the NYPD Complaint Dataset. He was majorly involved in creating spatial visualizations using two different mapping techniques (leaflet and cloud-based 	 ggmap).      
	
+ Joel is the presenter for the group. Ananya and Lichun worked on creating the Shiny App dashboard, and deploying the application. Joel, Rishav and Shanyue analyzed the data to create interactive visualization in the form of line chart and map demonstrations. As a whole, all team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

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

