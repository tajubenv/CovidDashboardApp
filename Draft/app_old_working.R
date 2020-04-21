#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse) ## Can you use R without it/.
library(RCurl) ## Used to import the data from github
library(stringr) ## Used for some minor string matching
library(lubridate) ## Used for dealing with dates
library(leaflet) ## Used for mapping
retrieve_covid_data <- function(){
  ## Retrieve all of the time series data from https://github.com/CSSEGISandData/COVID-19
  ## Retrieve Confirmed Cases
  x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
  confirmed_cases_csse <- read.csv(text = x)
  
  ## Retrieve Confirmed Deaths
  x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
  confirmed_deaths_csse <- read.csv(text = x)
  
  ## Retreive Recovered Case Data
  x <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
  confirmed_recovered_csse <- read.csv(text = x)
  
  ## Convert wide to long (Praise Hadley) and convert date to lubridate
  confirmed_cases_csse_gather <- confirmed_cases_csse %>%
    pivot_longer(-c(Province.State, Country.Region, Lat, Long), 
                 names_to = "Date", values_to = "Confirmed.Cases") %>% 
    mutate(cast_date = mdy(str_replace(Date, "X", ""))) %>% select(-Date)
  
  ## Convert wide to long (Praise Hadley) and convert date to lubridate
  confirmed_deaths_csse_gather <- confirmed_deaths_csse %>%
    pivot_longer(-c(Province.State, Country.Region, Lat, Long), 
                 names_to = "Date", values_to = "Confirmed.Deaths") %>% 
    mutate(cast_date = mdy(str_replace(Date, "X", ""))) %>% select(-Date)
  
  ## Convert wide to long (Praise Hadley) and convert date to lubridate
  confirmed_recovered_csse_gather <- confirmed_recovered_csse %>%
    pivot_longer(-c(Province.State, Country.Region, Lat, Long), 
                 names_to = "Date", values_to = "Recovered.Cases") %>% 
    mutate(cast_date = mdy(str_replace(Date, "X", ""))) %>% select(-Date)
  
  ## Join all data together
  csse_complete <- left_join(confirmed_cases_csse_gather, 
                             confirmed_deaths_csse_gather, 
                             by = c('Province.State', 'Country.Region',
                                    'Lat', 'Long', 'cast_date')) %>% 
    left_join(confirmed_recovered_csse_gather, by = c('Province.State', 'Country.Region',
                                                      'Lat', 'Long', 'cast_date')) %>% filter(Confirmed.Cases > 0)
  csse_complete <- csse_complete %>% 
    mutate(Confirmed.Cases.Text = paste0("Confirmed Cases: ", Confirmed.Cases),
           Confirmed.Deaths.Text = paste0("Confirmed Deaths: ", Confirmed.Deaths),
           Recovered.Cases.Text = paste0("Recovered Cases: ", Recovered.Cases))
  return(csse_complete)
}


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Covid Test App"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         uiOutput("date_slider"),
         selectInput("case_type",
                     "Select Visual",
                     c("Confirmed Cases",
                       "Confirmed Deaths",
                       "Recovered Cases"),
                     "Confirmed Cases")
         
         ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   csse_data <- retrieve_covid_data()
   tempzoom <- input$distPlot_zoom
   output$distPlot <- renderLeaflet({
     if(input$case_type == "Confirmed Cases"){
     leaflet() %>% addTiles() %>% 
       addCircles(data = filter(csse_data, cast_date == ymd(input$date)), 
                  lng = ~Long, lat = ~Lat, 
                  radius = ~sqrt(Confirmed.Cases) * 3000, 
                  label = ~htmltools::htmlEscape(Confirmed.Cases.Text),
                  color = '#7570b3') %>%
         setView(lng = tempzoom$lng, lat = tempzoom$lat, zoom = tempzoom$zoom)
     }
     else if(input$case_type == "Confirmed Deaths"){
       leaflet() %>% addTiles() %>% 
         addCircles(data = filter(csse_data, cast_date == ymd(input$date)), 
                    lng = ~Long, lat = ~Lat, 
                    radius = ~sqrt(Confirmed.Deaths) * 3000, 
                    label = ~htmltools::htmlEscape(Confirmed.Deaths.Text),
                    color = '#d95f02')
     }
     else if(input$case_type == "Recovered Cases"){
       leaflet() %>% addTiles() %>% 
         addCircles(data = filter(csse_data, cast_date == ymd(input$date)), 
                    lng = ~Long, lat = ~Lat, 
                    radius = ~sqrt(Recovered.Cases) * 3000, 
                    label = ~htmltools::htmlEscape(Recovered.Cases.Text),
                    color = '#1b9e77') 
     }
   })
   output$date_slider <- renderUI({
     sliderInput("date",
                 "Date:",
                 min = ymd(min(csse_data$cast_date)),
                 max = ymd(max(csse_data$cast_date)),
                 value = ymd(max(csse_data$cast_date))
   )
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

