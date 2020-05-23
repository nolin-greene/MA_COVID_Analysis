#The goal of this script is to learn how to build a simple Shiny app that displays a ggplot graph and 
#allows the user to filter the data being graphed by date inputs
#additional future goals: plotly, multiple tabs

library(shiny)
library(readr)
library(zoo)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(forcats)
library(gridExtra)

rm(list = ls())
county_all_dates<-read_csv("daily_ma_covid_cases_by_county.csv", col_types = "ciDi")
county_all_dates<-county_all_dates %>%
  filter(county != "Unknown")

#creates a simple df of new statewide positive tests
statewide<-county_all_dates %>%
  group_by(date) %>%
  summarise(new_cases = sum(new_cases))%>%
  mutate(new_cases_7d = rollmean(new_cases, k= 7, fill = NA))

#creates a simple df of county totals:
county_totals<-county_all_dates %>%
  group_by(county = as.factor(county)) %>%
  summarize(total_cases = sum(new_cases)) %>%
  filter(county != "Unknown")

county_totals$population<-as.integer(c(212990,124944,565217,17332,789034,70180,466372,160830,1611699,11399,706775,521202,803907,830622))
county_totals$per1000<-county_totals$total_cases/(county_totals$population/1000)
county_totals$county<-fct_reorder(county_totals$county, county_totals$total_cases)
county_totals$countyper<-fct_reorder(county_totals$county, county_totals$per1000)


ui<-fluidPage(
  titlePanel("Confirmed COVID-19 Cases in Massachusetts by County"),
  #we're creating a fluid page layout, although there are other layouts we could work with
  sidebarLayout(
    #this creates a layout within a sidebar
    sidebarPanel(
      #this creates a sidebar panel containing input controls
      dateRangeInput(inputId = "dates", 
                     label = "Select a date range:", 
                     start = "2020-03-16",
                     end = NULL, 
                     min = "2020-03-16", 
                     max=today(), 
                     format = "m/d/yyyy", 
                     weekstart = 0, 
                     language = "en",
                     width = "200px"),
      checkboxGroupInput(inputId = "counties", 
                         label="Select which counties to display:", 
                         choices = c(sort(unique(county_all_dates$county))), 
                         selected = c(sort(unique(county_all_dates$county)))),
      width = 3),
    mainPanel(
      #this configures the content on the main panel of the db - the contents will be created in the server function
      plotOutput(outputId = "facet_counties"),
      plotOutput(outputId = "counties_total"),  
      plotOutput(outputId = "counties_percap")
      #this gets created in the server function and will then be passed to the sidebar function
    )
  )
)

server<- function(input, output){
  #contains the instructions that are needed to build the app
  g1 <- reactive({
    filter(county_all_dates, county != "Unknown" & 
             between(date ,input$dates[1], input$dates[2]) &
             county %in% input$counties)
  })
  output$facet_counties <- renderPlot({
      ggplot(g1(),aes(x = date, y = new_cases))+
      geom_smooth(se = F)+
      facet_wrap(~county, scales = "free_y")+ 
      theme_hc()+
      theme(plot.title = element_text(face = "bold", size = 18),
            plot.subtitle = element_text(size = 14),
            panel.background = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            strip.background = element_rect(colour="black", fill="gray95"),
            strip.text.x = element_text(size = 14))+
      labs(title = "Daily New Confirmed Cases")
    #the variable on output - "scatterplot" corresponds to the output Id in the ui function above
    #and the aesthetics for x and y correspond to the inputIds in the ui function above
  })
  g2 <- reactive({
    filter(county_totals, 
             county %in% input$counties)
  })
  
  output$counties_total <- renderPlot({
    p1<- ggplot(g2(), aes( county, total_cases))+
    geom_bar(stat = "identity", fill = "slategrey")+
    scale_y_continuous(expand = c(0,2), breaks = seq(0,15000, by = 5000),limit = c(0,22000))+
    coord_flip()+
    labs(title =  "Total Cases")+
    theme(plot.title = element_text(face = "bold", size = 18),
          axis.text.y = element_text(size= 14, face = "bold"),
          axis.text.x = element_text(size= 14, face = "bold"),
          axis.ticks.y= element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          panel.background = element_blank())
  
    p2<- ggplot(g2(), aes(countyper, per1000))+
      geom_bar(stat = "identity", fill = "slategrey")+
      scale_y_continuous(expand = c(0,0), breaks = seq(0,16, by = 4),limit = c(0,22))+
      coord_flip()+
      labs(title = "Total Cases per 1000 Residents", 
           caption = "Source: MA Department of Public Health")+
      theme(plot.title = element_text(face = "bold", size = 18),
            axis.ticks.y= element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            axis.text.y = element_text(size= 14, face = "bold"),
            axis.text.x = element_text(size= 14, face = "bold"),
            panel.background = element_blank()
      )
    grid.arrange(p1,p2,ncol=2,top="")
  })
}

shinyApp(ui, server)