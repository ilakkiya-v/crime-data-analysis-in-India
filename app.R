#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dash)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(readr)
library(tidyverse)
library(treemap)
library(sunburstR)
library(tibble)
library(treemapify)
#DATASETS----
#D2017 <- read_csv("/cloud/project/2017.csv")
#D2018 <- read_csv("/cloud/project/2018.csv")
#D2019 <- read_csv("/cloud/project/2019.csv")
#D2020 <- read_csv("/cloud/project/2020.csv")

#Merging the dataset 
#d17 <- D2017%>%filter(District=="Total")
#d18 <- D2018%>%filter(District=="Total")
#d19 <- D2019%>%filter(District=="Total")
#d20 <- D2020%>%filter(District=="Total Districts")
#d17$Year=2017
#d18$Year=2018
#d19$Year=2019
#d20$Year=2020
#data <-bind_rows(d17,d18,d19,d20)

data <- read.csv("data.csv")
w <- read.csv("crime against women.csv", row.names=NULL)
tn <- read.csv("final/taminadu.csv")
tn_new <- read.csv("final/taminadu1.csv")


#mean <- aggregate(x = data$Murder,            
#          by = list(data$Year),            
#          FUN = sum) 

#years_total_df <- data[,4:40] %>% group_by(data$Year) %>% summarize_all(funs(sum))
#years_total_df <- t(years_total_df)
#years_total_df <- years_total_df[-1,]
#colnames(years_total_df) <- c(2017,2018,2019,2020)
#years_total_df <- as.data.frame(years_total_df)
#years_total_df <- rownames_to_column(years_total_df,var = "Crime")

con <- select(data,State,Year,Murder,Dowry.Deaths,Hurt,Assault.on.Women,Sexual.Harassment,Stalking,Kidnapping.and.Abduction,Rape,Theft,Circulate.False.Fake.News.Rumours,Insult.to.the.Modesty.of.Women)
contn <- select(tn,District,Year,Hurt,Offences.Affecting.the.Human.Body,Theft,Rash.Driving.on.Public.way,Miscellaneous.IPC.Crimes,Total.Cognizable.IPC.crimes,Robbery,Obstruction.on.Public.way,Assault.on.Women,Cruelty.by.Husband.or.his.Relatives,Rape,Circulate.False.Fake.News.Rumours,Theft)


tn_total <- select(tn_new,District,Year,Hurt,Offences.Affecting.the.Human.Body,Theft,Rash.Driving.on.Public.way,Miscellaneous.IPC.Crimes,Total.Cognizable.IPC.crimes,Robbery,Obstruction.on.Public.way,Assault.on.Women,Cruelty.by.Husband.or.his.Relatives,Rape,Circulate.False.Fake.News.Rumours,Theft)

years_total_df <- con[,2:13] %>% group_by(Year) %>% summarize_all(funs(sum))
colnames(years_total_df) <- c("Year", "Murder","Dowry","Hurt", "Assaultw", "SexHar","Stalking", "KidnapAbd","Rape","Theft","Fakenews","Insultw")

years_total_df$Crime <- rowSums(years_total_df[ , c(2:12)],na.rm=TRUE)


yearly_crime_df <- data.frame(Year=integer(), Crime=integer(), Number=integer())
for (row in 1:nrow(years_total_df)){
  year <- years_total_df[row, "Year"]
  murder <- years_total_df[row, "Murder"]
  dowry <- years_total_df[row, "Dowry"]
  hurt <- years_total_df[row, "Hurt"]
  assaultw <- years_total_df[row, "Assaultw"]
  sexhar <- years_total_df[row, "SexHar"]
  stalking <- years_total_df[row, "Stalking"]
  kidnapabd <- years_total_df[row, "KidnapAbd"]
  rape <- years_total_df[row, "Rape"]
  theft <- years_total_df[row, "Theft"]
  fakenews <- years_total_df[row, "Fakenews"]
  insultw <- years_total_df[row, "Insultw"]
  
  
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Murder", murder)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Dowry", dowry)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Hurt", hurt)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Assaultw", assaultw)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "SexHar", sexhar)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Stalking", stalking)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "KidnapAbd", kidnapabd)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Rape", rape)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Theft", theft)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Fakenews", fakenews)
  yearly_crime_df[nrow(yearly_crime_df) + 1,] = c(year, "Insultw", insultw)
  
}

bar_chart_total_crimes <- ggplot(data=years_total_df, aes(x=Year, y=Crime, fill=factor(Year))) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=Crime), vjust=2.6, color="white", size=10) 
labs(fill = "Year")


# ------------ TAMIL NADU ------------#

#Hurt cases
tn1 <- contn %>%
  plot_ly(
    x = ~Hurt, 
    y = ~District, 
    size = ~Hurt, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Hurt Cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Offences Affecting the Human Body cases
tn2 <- contn %>%
  plot_ly(
    x = ~Offences.Affecting.the.Human.Body, 
    y = ~District, 
    size = ~Offences.Affecting.the.Human.Body, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Cases of Offences Affecting the Human Body: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )


#Theft cases
tn3 <- contn %>%
  plot_ly(
    x = ~Theft, 
    y = ~District, 
    size = ~Theft, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Cases of Theft: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Rash Driving on Public way cases
tn4 <- contn %>%
  plot_ly(
    x = ~Rash.Driving.on.Public.way, 
    y = ~District, 
    size = ~Rash.Driving.on.Public.way, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>  #Rash Driving on Public way cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Cruelty.by.Husband.or.his.Relatives cases
tn5 <- contn %>%
  plot_ly(
    x = ~Cruelty.by.Husband.or.his.Relatives, 
    y = ~District, 
    size = ~Cruelty.by.Husband.or.his.Relatives, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>  #Cruelty by Husband or his Relatives cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Obstruction on Public way cases
tn6 <- contn %>%
  plot_ly(
    x = ~Obstruction.on.Public.way, 
    y = ~District, 
    size = ~Obstruction.on.Public.way, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>  #Obstruction on Public way cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Circulate.False.Fake.News.Rumours
tn7 <- contn %>%
  plot_ly(
    x = ~Circulate.False.Fake.News.Rumours, 
    y = ~District, 
    size = ~Circulate.False.Fake.News.Rumours, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>  #Fake news cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  ) 


#Total IPC cases
tn8 <- contn %>%
  plot_ly(
    x = ~Total.Cognizable.IPC.crimes, 
    y = ~District, 
    size = ~Total.Cognizable.IPC.crimes, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Cases of Offences Affecting the Human Body: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~District
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )


#-------------------------------------# 


#Murder Cases
f1 <- data %>%
  plot_ly(
    x = ~Murder, 
    y = ~State, 
    size = ~Murder, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'scatter',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Murder Cases: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )


#Dowry Deaths
x1 <- data$Dowry.Deaths
f2 <- data %>%
  plot_ly(
    x = ~x1, 
    y = ~State, 
    size = ~x1, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Dowry Deaths: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )


#Hurt
f3 <- data %>%
  plot_ly(
    x = ~Hurt, 
    y = ~State, 
    size = ~Hurt, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Hurt: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Assault on Women
x2 <- data$Assault.on.Women
f4 <- data %>%
  plot_ly(
    x = ~x2, 
    y = ~State, 
    size = ~x2, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Assault on Women: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Sexual Harassment
x4 <- data$Sexual.Harassment
f5 <- data %>%
  plot_ly(
    x = ~x4, 
    y = ~State, 
    size = ~x4, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Sexual Harassment: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Stalking
x3 <- data$Stalking
f6 <- data %>%
  plot_ly(
    x = ~x3, 
    y = ~State, 
    size = ~x3, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Stalking: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Kidnapping and Abduction
x5 <- data$Kidnapping.and.Abduction
f7 <- data %>%
  plot_ly(
    x = ~x5, 
    y = ~State, 
    size = ~x5, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Kidnapping and Abduction: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )
#Rape
x6 <- data$Rape
f8 <- data %>%
  plot_ly(
    x = ~x6, 
    y = ~State, 
    size = ~x6, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Rape: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Theft
x7 <- data$Theft
f9 <- data %>%
  plot_ly(
    x = ~x7, 
    y = ~State, 
    size = ~x7, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Theft: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Fake News Rumours
x8 <- data$Circulate.False.Fake.News.Rumours
f10 <- data %>%
  plot_ly(
    x = ~x8, 
    y = ~State, 
    size = ~x8, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Circulation of fake news rumours: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#Insult to Modesty of Women
x9 <- data$Insult.to.the.Modesty.of.Women
f11 <- data %>%
  plot_ly(
    x = ~x9, 
    y = ~State, 
    size = ~x9, 
    frame = ~Year, 
    hoverinfo = "text",
    type = 'bar',
    mode = 'markers',
    hovertemplate = paste(
      "<b>State: %{y}<br></b>",
      "<b>Insult to Modesty of Women: %{x}<br></b>",
      "<extra></extra>"
    ),
    color = ~State
  ) %>%
  layout(
    xaxis = list(
      type = "log"
    )%>%animation_opts(1)
  )

#--------Crime against women----------

wp1<-plot_ly(x=w$State,y=w$Dowry.Deaths,data=w,type="bar") %>%
  layout(title = "Dowry Death Crimes",
         xaxis = list(title = "State/ Union Terrirtory",
                      zeroline = FALSE),
         yaxis = list(title = "Number of crimes",
                      zeroline = FALSE))

ww <- plot_ly(
  labels=c("Crime Against Women","Dowry Deaths","Cruelty by Husband or his relatives","Kidnapping & Abduction of Women",
           "Kidnapping of women for Ransom","Kidnapping & Abduction of Women to compel her for marriage","Kidnapping & Abduction of Women (above 18 yrs) to compel her for marriage",
           "Kidnapping & Abduction of Women (below 18 yrs) to compel her for marriage","Procuration of Minor Girls","Selling of Minor Girls",
           "Rape (Total)","Rape Women (18 Yrs and above)","Rape Women (18 Yrs below)","Assault on Women with Intent to Outrage her Modesty Total",
           "Assault on Women above 18 with Intent to Outrage her Modesty","Assault on Women below 18 with Intent to Outrage her Modesty","Insult to the Modesty of Women Total","Insult to the Modesty of Women above 18",
           "Insult to the Modesty of Women below 18"),
  
  parents=c("","Crime Against Women","Crime Against Women","Crime Against Women","Kidnapping & Abduction of Women","Kidnapping & Abduction of Women",
            "Kidnapping & Abduction of Women to compel her for marriage","Kidnapping & Abduction of Women to compel her for marriage","Crime Against Women",
            "Procuration of Minor Girls","Crime Against Women","Rape (Total)","Rape (Total)","Crime Against Women","Assault on Women with Intent to Outrage her Modesty Total",
            "Assault on Women with Intent to Outrage her Modesty Total","Crime Against Women","Insult to the Modesty of Women Total","Insult to the Modesty of Women Total"),
  values=c("",7045,112292,63693,59,25098,13862,11236,2480,12,28153,25498,2655,86745,82608,4137,7250,7118,132),
  type='sunburst'
)


ui <- dashboardPage(
  dashboardHeader(title = "Crime Data Analysis"),
  dashboardSidebar( sidebarMenu(
    menuItem("Description", tabName = "home", icon = icon("home")),
    menuItem("Tamil Nadu", tabName = "tn", icon = icon("bar-chart-o")),
    menuItem("Data Analysis", tabName = "data", icon = icon("play")),
    menuItem("Crime Against Women", tabName = "women", icon = icon("dashboard")),
    menuItem("Conclusion",tabName = "inference",icon=icon("table"))
  )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName ="home",tags$h1("CRIME DATA ANALYSIS"),
        tags$h4("Crime is a very old concept and it is transmitted to the society from generation to generation. Crime produces law and order situation. It is a social evil. It is generated by the society and the society also suffers a lot because of crime committed by its members. The rising wave of crime to-day has caused alarm in the public.
                With the rapid improvement of lifestyle and urbanization, the graph of crimes is also on the increase.
                As the report says that India has high illiteracy rate, high population density, low job opportunities, these have become one of the reasons for high crime rate in India. Due to different problems in every state, different states have different crime rate. In this project, we analyze the patterns of crime performed on a dataset with demographic information of crime in India(State-Wise & District-Wise)."),
        tags$br(),
        
        tags$h2("DATASET"),
        tags$h4("First we obtained the dataset from the",tags$strong("NCRB Website"),"We performed data cleaning and data wrangling."),
        tags$h4("Then we performed the exploratory data analysis on the datasets that we cleaned."),
        tags$br(),
        tags$h2("EXPLORATORY DATA ANALYSIS"),
        tags$h4("Visualization of data is the appearance of data in a pictographic or graphical form. This form facilitates top management to understand the data visually and get the messages of difficult concepts or identify new patterns. The approach of the personal understanding to handle data; applying diagrams or graphs to reflect vast volumes of complex data is more comfortable than presenting over tables or statements. In this study, we conduct data processing and data visualization for crime report data that occurred in the India in the range of 2017 to 2020 using R language. As the result shows, by using those methods, 
                we can gain insights, understandings, new patterns, and do visual analytics from the existing data. "),
        tags$br(),
        tags$h2("INSPIRATION"),
        tags$h4("The motivation behind taking up this topic for the research is that every aware
citizen in today's modern world wants to live in a safe environment and neighborhood.
However it is a known fact that crime in some form, exists in our society. Although
we cannot control what goes on around us, we can definitely try to take a few steps to
aid the government and police authorities in trying to control it.Hence, taking inspiration from the facts stated above, we decided to
process this data provided and analyze it to identify the trends in crime over the years")
        
      ),
      tabItem(
        tabName ="tn",tags$h1("Tamil Nadu: District-wise Analysis"),
        fluidRow(column(9,selectInput("cr1","Choose the Crime:",choices=c("Hurt","Offences affecting human body","Rash Driving","Obstruction on public way",
                                                                          "Cruelty by Husband or his Relatives","Theft","Circulate False Fake News Rumours",
                                                                          "Total IPC Crimes"
        )))),
        fluidRow(plotlyOutput("Plot3"),
                 
                 tags$br(),
                 tags$h2("OVERALL HURT CASES"),
                 tags$h4("According the given plot during 2017, nagapattinam has the highest cases of hurt after which Kanchipuram followed. Thirunelveli was in third place.
                    During the year of 2018, the top three districts with high hurt cases are thirunelveli, chennai and vilupuram.
                    During the year of 2019, the top three districts with high theft cases are thirunelveli, chennai, kanyakumari.
                    During the year of 2020, the top three districts with high theft cases are chennai, railway chennai and kanyakumari"),
                 
                 tags$br(),
                 
                 tags$h2("OVERALL BODILY OFFENCE CASES"),
                 tags$h4("According the given plot during 2017, Chennai has the highest cases of hurt after which vellore followed. Kanchipuram was in third place.
                    During the year of 2018, the top three districts with high hurt cases are chennai,thirunelveli and vilupuram.
                    During the year of 2019, the top three districts with high theft cases arehennai,thirunelveli and vilupuram.
                    During the year of 2020, the top three districts with high theft cases are chennai, cuddalore and dindugal"),
                 
                 tags$br(),
                 
                 tags$h2("OVERALLRASH DRIVING CASES"),
                 tags$h4("According the given plot during 2017, Chennai has the highest cases of hurt after which cuddalore followed. villupuram was in third place.
                    During the year of 2018, the top three districts with high hurt cases are same as 2017.
                    During the year of 2019, the top three districts with high theft cases are chennai, villupuram and cuddalore.
                    During the year of 2020, the top three districts with high theft cases are chennai, cuddalore and tiruppur"),
                 
                 tags$br(),
                 
                 tags$h2("OVERALL THEFT"),
                 tags$h4("According the given plot during 2017, chennai has the highest cases of theft after which Railway Chennai followed. Madurai city was in third place after which Vellore follows with fourth place.
                    During the year of 2018, the top three districts with high theft cases are chennai, railway chennai and railway trichy.
                    During the year of 2019, the top three districts with high theft cases are same the districts during 2018.
                    During the year of 2020, the top three districts with high theft cases are cuddalore, chennai and dindigul"),
                 
                 
                 tags$br(),
                 
                 tags$h2("TOTAL IPC CASES"),
                 tags$h4("According the given plot during 2017, Chennai has the highest cases of hurt after which Villupuram followed. vellore was in third place.
                    During the year of 2018, the top three districts with high hurt cases are Chennai, vellore and Villupuram.
                    During the year of 2019, the top three districts with high theft cases are chennai, thirunelveli and vellore.
                    During the year of 2020, the top three districts with high theft cases are chennai, chengalpattu and thiruvannamalai"),
                 
                 
                 
                 
                 
                 
                 
                 
                 
        ),
        
        fluidRow(
          
          box(
            title = paste("Individual Crime Type over the Years 2017-2020") , status ="success", background="black", width = 15,
            plotlyOutput(("linegraph1"), height="500px")
          ) 
          
          
        )
        
        
      ),
      tabItem(tabName = "data",tags$h1("Exploratory Data Analysis : Plots"),
              fluidRow(
                
                box(
                  width = 6, height = "428px",
                  
                  valueBox(29, "States", icon = icon("landmark"), color="blue"),
                  
                  valueBox(8, "Union Territories", icon = icon("bell"), color = "olive"),  
                  
                  valueBox(11, "Crime Types", icon = icon("gopuram"), color = "yellow"),  
                  
                  
                  tags$br(),
                  
                  tags$h4("We have analyzed over 11 crime types across all the States and Union Territories of India. We have also analyzed the crime types over these years individually."),tags$br(),
                  tags$h4("The line chart of Individual crime type over the years 2017-2020 shows the number of cases in each type of crime
                          and the aggregated total crimes over the 4 years."),tags$br(),
                  tags$h4("Cases due to murder,hurt, fake news, insult to the modesty of women increased especially from 2019 to 2020. Cases due to dowry deaths, assault on women, sexual harassment,
                          stalking, kidnapping and abduction, rape, theft decreased in the pandemic period (2020).")
                  
                ),
                
                box(
                  title = paste("Individual Crime Type over the Years 2017-2020") , status ="success", background="black", width = 6,
                  plotlyOutput(("linegraph"), height="365px")
                )
                
              ),
              fluidRow(
                box(
                  title = "Crime over the Years", status ="success", width= 6,
                  plotOutput(outputId = "barchart", height = "350px", hover="plot_hover"), tags$br(),
                  tags$h4("This bar plot shows the total number of cases over the 4 years. We can see that the cases were increased steadily from 2017 to 2019 and decreased in 2020 due to the pandemic year.")
                ),
                box(
                  title = paste("Crime Breakdown") , status ="success", width =6,
                  plotOutput(outputId = "treemap", height = "350px"), tags$br(),
                  tags$h4("The treemap plot gives us an idea about the majorly occuring crime, we can see that theft cases is the highest, second highest is hurt, third highest is kidnapping and abduction and so on.")
                )
              ),
              fluidRow(column(9,selectInput("cr","Choose the Crime:",choices=c("Murder","Dowry","Hurt","Assault on Women",
                                                                               "Sexual Harassment","Stalking","Kidnapping and Abduction",
                                                                               "Rape","Theft","Circulate False Fake News Rumours",
                                                                               "Insult to the Modesty of Women")))),
              fluidRow(plotlyOutput("Plot1")),tags$br(),
              tags$h4("This animated bar plot shows us the crimes cases in each state and union territories over the 4 years from 2017 to 2020."),
              tags$br(),
              tags$h2("Muder Cases"),
              tags$h4("According to the plot during 2017,2018,2019,2020 Uttar Pradesh has the highest cases of murder after which Bihar followed.Maharashtra was in third place. "),
              tags$br(),
              tags$h2("Dowry Death Cases"),
              tags$h4("According the plot during 2017,2018,2019,2020 Uttar Pradesh has the highest cases of dowry deaths after which Bihar followed.Madhya Pradesh was in third place.  "),
              tags$br(),
              tags$h2("Hurt Cases"),
              tags$h4("According to the plot during 2017,Bihar has the highest cases of hurt after which Madhya Pradesh followed.Tamil Nadu was in third place. "),
              tags$h4("According to the plot during 2018,2019,2020 Bihar has the highest cases of hurt after which Madhya Pradesh followed.Uttar Pradesh was in third place. "),
              tags$br(),
              tags$h2("Assault on Women"),
              tags$h4("According to the plot during 2017 Odisha has the highest cases of assault after which Madhya Pradesh followed.Uttar Pradesh was in third place. "),
              tags$h4("During 2018,Odisha has the highest cases of assault after which Maharashtra followed.Madhya Pradesh was in third place. "),
              tags$h4("During 2019,Rajasthan has the highest cases of assault after which Odisha followed.Uttar Pradesh was in third place. "),
              tags$h4("During 2020,Odisha has the highest cases of assault after which Rajasthan  followed.Maharashtra was in third place. "),
              tags$br(),
              tags$h2("Sexual Harassment"),
              tags$h4("According to the plot during 2017 Uttar Pradesh has the highest cases of sexual harassment after which Madhya Pradesh followed.Maharashtra was in third place. "),
              tags$h4("During 2018,2019,2020 Uttar Pradesh has the highest cases of sexual harassment after which Maharashtra followed.Madhya Pradesh was in third place. "),
              tags$br(),
              tags$h2("Stalking"),
              tags$h4("According to the plot during 2017,2018,2019 Maharashtra has the highest cases of stalking after which Telangana followed.Madhya Pradesh was in third place. "),
              tags$h4("During 2020 Maharashtra has the highest cases of stalking after which Telangana  followed.Andhra Pradesh was in third place. "),
              tags$br(),
              tags$h2("Kidnapping and Abduction"),
              tags$h4("According to the plot during 2017,2018,2019 Uttar Pradesh has the highest cases of kidnapping and abduction after which Maharashtra followed.Bihar was in third place. "),
              tags$h4("During 2020 Uttar Pradesh has the highest cases of kidnapping and abduction after which West Bengal  followed.Maharashtra was in third place. "),
              tags$br(),
              tags$h2("Rape"),
              tags$h4("According to the plot during 2017 Madhya Pradesh has the highest cases of rape after which Uttar Pradesh followed.Rajasthan was in third place. "),
              tags$h4("During 2018 Madhya Pradesh has the highest cases of rape after which Rajasthan followed.Uttar Pradesh was in third place. "),
              tags$h4("During 2019,2020 Rajasthan has the highest cases of rape after which Uttar Pradesh followed.Madhya Pradesh was in third place. "),
              tags$br(),
              tags$h2("Theft"),
              tags$h4("According to the plot during 2017,2018,2019,2020 Delhi has the highest cases of theft after which Maharashtra followed.Uttar Pradesh was in third place. "),
              tags$br(),
              tags$h2("Circulation of fake news rumours"),
              tags$h4("According to the plot during 2017 Madhya Pradesh has the highest cases of rumours after which Uttar Pradesh followed.Kerala was in third place. "),
              tags$h4("During 2018 Madhya Pradesh has the highest cases of rumours after which Tamil Nadu and Telangana followed.Maharashtra was in third place. "),
              tags$h4("During 2019 Tamil Nadu has the highest cases of rumours after which Uttar Pradesh followed.Madhya Pradesh was in third place. "),
              tags$h4("During 2020 Telangana has the highest cases of rumours after which Telangana followed.Uttar Pradesh was in third place. "),
              tags$br(),
              tags$h2("Insult to the modesty of women"),
              tags$h4("According to the plot during 2017 Andhra Pradesh has the highest cases of insult after which Telangana followed.Maharashtra was in third place. "),
              tags$h4("During 2018,2019 Andhra Pradesh has the highest cases of insult after which Maharashtra followed.Telangana was in third place. "),
              tags$h4("During 2020 Andhra Pradesh has the highest cases of insult after which Maharashtra followed.Odisha was in third place. "),
              tags$br(),
              
              
      ),
      tabItem(
        tabName = "women",tags$h1("CRIME AGAINST WOMEN"),
        fluidRow(plotlyOutput("Plot2")),tags$br(),
        tags$h4("The sunburst graph shows the cases of crime against women in the year 2020. The highest is cruelty by husband  or his relatives, second highest is assault on women with intent to outrage her modesty out of which 82,608 cases were of women with age above 18 and 4137 cases of women with age below 18.
                The third highest is cases with kidnapping and abduction of women out of which 25098 cases were due to compelling the women for marriage. The cases with above 18 are 13,862 and the cases with below 18 are 11,236.
                The fourth highest is rape, out of which 25498 cases are women of above 18 and 2655 cases of women with below 18 age group.
                The fifth highest is insult to the modesty of women.
                The sixth hight is cases due to dowry deaths.
                The least number of cases is of procuration of minor girls.")
        
      ),
      
      tabItem(
        tabName = "inference",tags$h1("CONCLUSION"),
        tags$h4("We conclude by saying that with all the visualizations it can be noted that the pandemic year i.e",tags$strong("2020"),"has reduced the number of crimes occuring.",tags$br(),"But it should be noted that some of the crimes like murder, hurt, fake news rumours and insult to modesty of a woman has increased and cases due to dowry deaths, assault on women, sexual harassment, stalking, kidnapping and abduction, rape, theft decreased in the pandemic period (2020) due to the lockdown imposed in the country."),
        
      )
      
    ))
  ,skin=c("purple"))

server <- function(input, output,session) { 
  
  output$linegraph <- renderPlotly({
    years_total_df%>%plot_ly(x=~Year)%>%
      add_lines(y=years_total_df$Crime,name="Total Crimes",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Murder,name="Murder",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Dowry,name="Dowry",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Hurt,name="Hurt",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Assaultw,name="Assault on women",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$SexHar,name="Sexual Harassment",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Stalking,name="Stalking",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$KidnapAbd,name="Kidnapping and Abduction",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Rape,name="Rape",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Theft,name="Theft",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Fakenews,name="Fake News",line= list(shape = "line"))%>%
      add_lines(y=years_total_df$Insultw,name="Insult to the modesty of Women",line= list(shape = "line"))
    
  })
  
  
  output$linegraph1 <- renderPlotly({
    tn_total%>%plot_ly(x=~Year)%>%
      add_lines(y=tn_total$Miscellaneous.IPC.Crimes,name="Total Crimes",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Offences.Affecting.the.Human.Body,name="body offences",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Theft,name="Theft",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Hurt,name="Hurt",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Rash.Driving.on.Public.way,name="Rash Driving",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Robbery,name="Robbery",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Obstruction.on.Public.way,name="Obstruction",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Assault.on.Women,name="Assault on women",line= list(shape = "line")) %>%
      add_lines(y=tn_total$Cruelty.by.Husband.or.his.Relatives,name="Cruelty by husbands",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Rape,name="Rape",line= list(shape = "line"))%>%
      add_lines(y=tn_total$Circulate.False.Fake.News.Rumours,name="False news rumours",line= list(shape = "line"))
  })
  
  output$barchart <- renderPlot(bar_chart_total_crimes)
  
  output$treemap <- renderPlot({
    total_year <- yearly_crime_df %>% group_by(`Crime`) %>% summarize_all(funs(sum), na.rm=TRUE)
    #print(crime_total_df)
    print(total_year)
    ggplot(total_year, aes(fill= `Crime`, area=`Number`)) + 
      geom_treemap() +
      geom_treemap_text(colour = "white", place="centre", label=paste(total_year$Crime,": ",total_year$Number)) +
      labs(title=paste("Aggregated Crime Distribution for 2017-2020")) +
      theme(legend.position="right")  +
      scale_fill_brewer(palette="Paired")
  })
  
  
  
  output$Plot1<- renderPlotly({
    if(input$cr=="Murder"){
      subplot(f1,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Dowry"){
      subplot(f2,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Hurt"){
      subplot(f3,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Assault on Women"){
      subplot(f4,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Sexual Harassment"){
      subplot(f5,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Stalking"){
      subplot(f6,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Kidnapping and Abduction"){
      subplot(f7,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Rape"){
      subplot(f8,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Theft"){
      subplot(f9,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Circulate False Fake News Rumours"){
      subplot(f10,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr=="Insult to the Modesty of Women"){
      subplot(f11,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
  }
  )
  
  output$Plot3<- renderPlotly({
    if(input$cr1=="Hurt"){
      subplot(tn1,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Offences affecting human body"){
      subplot(tn2,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Rash Driving"){
      subplot(tn4,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Obstruction on public way"){
      subplot(tn6,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Cruelty by Husband or his Relatives"){
      subplot(tn5,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Theft"){
      subplot(tn3,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Circulate False Fake News Rumours"){
      subplot(tn7,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
    else if(input$cr1=="Total IPC Crimes"){
      subplot(tn8,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    }
  }
  )
  
  output$Plot2<- renderPlotly({
    subplot(ww,nrows=1,widths=1,shareX = TRUE,shareY = FALSE,titleY = TRUE)
    
  }
  )
  
  
}
shinyApp(ui, server)
