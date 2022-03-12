library(readr)
library(shiny)
library(tidyverse)
library(ggridges)
library(ggplot2)
library(plotly)
library(scales)
df0=read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_deaths.csv")
df1=read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_deaths_age.csv")
df2=read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-11-13/malaria_inc.csv")
df <- df1 %>% 
  mutate(Entity=entity,Code=code,Year=year) %>% 
  transmute(Entity,Code,Year,age_group,deaths) %>% 
  left_join(df0) %>% 
  mutate(Deaths100k=`Deaths - Malaria - Sex: Both - Age: Age-standardized (Rate) (per 100,000 people)`) %>%
  left_join(df2) %>% 
  mutate(Incidence=`Incidence of malaria (per 1,000 population at risk) (per 1,000 population at risk)`) %>% 
  transmute(Entity,Year,age_group,deaths,Deaths100k,Incidence) %>% 
  mutate(age_group=factor(age_group,levels = c("Under 5","5-14","15-49","50-69","70 or older"))) %>% 
  group_by(Entity) %>% 
  filter(!any(deaths==0))


data_generation<-function(age,land,year1,year2){
  df %>% 
    filter(age_group%in%age) %>% 
    filter(Entity%in%land) %>% 
    filter(Year>=year1) %>% 
    filter(Year<=year2)
}
deathsplotter<-function(df5){
  ggplot(df5, aes(x=Year, fill = age_group, y = deaths)) +
    geom_density(alpha = 0.7, bw = .12, position = "stack",stat="identity") +
    scale_fill_brewer(palette = "Set2") +
    facet_grid(Entity ~ .)+
    labs(y="",yaxt="n",title="Total Deaths")+
    theme(legend.position = "top",axis.ticks.x=element_blank(),axis.ticks.y = element_blank(),
          panel.background = element_rect(fill="white"))+
    scale_y_continuous(labels = unit_format(unit = "million",scale=1e-6))
}

bar<-function(df5){
  plot<-ggplot(df5,aes(Year,Deaths100k,fill=Entity))+
    geom_bar(position = position_dodge(),stat = "identity")+
    labs(y="",yaxt="n",title="Deaths per 100,000 People")+
    theme(legend.position = "top",axis.ticks.x=element_blank(),axis.ticks.y = element_blank(),
          panel.background = element_rect(fill="white"))
  ggplotly(plot)
}
trends_table_fun<-function(df5){
  tableout<-matrix(data = NA,nrow = length(unique(df5$Entity)),ncol=2)
  for(i in 1:length(unique(df5$Entity))){
    tableout[i,1]=unique(df5$Entity)[i]
    temp = df5 %>% 
      filter(Entity%in%unique(df5$Entity)[i]) %>% 
      mutate(age_group=as.character(age_group)) %>% 
      transmute(Year,age_group,deaths) %>% 
      xtabs(formula=deaths~age_group+Year)
    tableout[i,2]=as.numeric(chisq.test(temp)[3])
  }
  tableout<-as.data.frame(tableout)
  colnames(tableout)<-c("Nation/Region","P-value of Deaths")
  tableout 
}

ui<-fluidPage(
  titlePanel("Total Deaths and Death per 100,000 people inflicted by Malaria"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose age groups, nations/regions, and years to explore total malaria deaths and death rate per 100,000 people. Also, look at the table to explore the p-value for the test of independence between age groups and years for total deaths for each country:"),
      selectInput("age", label = "Age Group", choices = c("Under 5","5-14","15-49","50-69","70 or older"),multiple=T,selected=unique(df$age_group)),
      selectInput("land",label = "Country/Region", choices = unique(df$Entity),selected=c("Sub-Saharan Africa","South Asia"),multiple=T),
      sliderInput("Year",label = "Year", min=1990, max=2016, value=c(1990,2016))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("plot",plotOutput("death")),
        tabPanel("bar",plotlyOutput("100k")),
        tabPanel("table",dataTableOutput("table"))
      )
    )
  )
)
server<-function(input,output){
  malaria_subset<-reactive({
    data_generation(input$age,input$land,input$Year[1],input$Year[2])
  })
  output$death<-renderPlot({deathsplotter(malaria_subset())})
  output$`100k`<-renderPlotly({bar(malaria_subset())})
  output$table<-renderDataTable({trends_table_fun(malaria_subset())})
}
shinyApp(ui,server)
