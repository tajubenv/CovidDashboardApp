## Covid tracking dashboard using shiny and leaflet
## Data is pulled directly from the 
## Johns Hopkins University Center for Systems Science and Engineering github
## Author: Tyler Jubenville
##

library(tidyverse) ## Can you use R without it/.
library(RCurl) ## Used to import the data from github
library(stringr) ## Used for some minor string matching
library(lubridate) ## Used for dealing with dates
library(leaflet) ## Used for mapping
library(shiny) ## Creates interactive dashboard
library(shinythemes) ## Makes Dashboard pretty

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
  
  ## Join all data together and removes 0 rows for confirmed cases
  csse_complete <- left_join(confirmed_cases_csse_gather, 
                             confirmed_deaths_csse_gather, 
                             by = c('Province.State', 'Country.Region',
                                    'Lat', 'Long', 'cast_date')) %>% 
    left_join(confirmed_recovered_csse_gather, by = c('Province.State', 'Country.Region',
                                                      'Lat', 'Long', 'cast_date')) %>% 
    filter(Confirmed.Cases > 0) 
  
  ## Create Character Variables for marker output later
  csse_complete <- csse_complete %>% 
    mutate(Confirmed.Cases.Text = paste0("Confirmed Cases: ", Confirmed.Cases),
           Confirmed.Deaths.Text = paste0("Confirmed Deaths: ", Confirmed.Deaths),
           Recovered.Cases.Text = paste0("Recovered Cases: ", Recovered.Cases))
  return(csse_complete)
}

top_n_countries <- function(data, n){
  ## Retreive top 10 countries for covid19 cases from csse data
  top_n <-data %>% 
    group_by(Country.Region) %>% 
    summarize(max_cases = max(Confirmed.Cases)) %>% 
    arrange(desc(max_cases)) %>% head(n) %>% 
    pull(Country.Region)
  return(top_n)
}

calculate_cum <- function(data, field){
  ## Calculate cumulative cases for plot
  output <- data %>% filter(field > 0) %>% 
    mutate(date_char = as.character(cast_date)) %>% 
    group_by(Country.Region, date_char) %>% 
    summarise(cum_cases = sum(get(field))) %>% 
    mutate(cast_date = ymd(date_char))
  
  return(output)
}


## Define UI for dashboard
ui <- fluidPage(
   ## Set theme
   theme = shinytheme("darkly"),
   ## Application title
   titlePanel("Covid19 Tracking Dashboard"),
   
   ## Sidebar definition
   sidebarLayout(
     
     ## Sidebar Panel definition
      sidebarPanel(
         ## Slider to select date 
         uiOutput("date_slider"),
         ## Selector for graph type
         selectInput("case_type",
                     "Select Visual",
                     c("Confirmed Cases",
                       "Confirmed Deaths",
                       "Recovered Cases"),
                     "Confirmed Cases"),
         ## Country Selector for cumulative plot
         uiOutput("country_selector")
         ),
      
      # Main Panel definition
      mainPanel(
         ## Output leaflet plot
         leafletOutput("distPlot"),
         ## Output reference to data
         uiOutput("reference"),
         ## Output interactive graph
         plotOutput("cumulative_graph")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
   ## Call function to retrieve data on startup
   csse_data <- retrieve_covid_data()
   
   ##  Define Date Slider 
   output$date_slider <- renderUI({
     sliderInput("date",
                 "Date:",
                 min = ymd(min(csse_data$cast_date)),
                 max = ymd(max(csse_data$cast_date)),
                 value = ymd(max(csse_data$cast_date))
     )
   })
   
   ## Define country selector based on countries with top 10 cases
   output$country_selector <- renderUI({
     checkboxGroupInput("countries",
                        "Cases Plot Countries:",
                        choices = top_n_countries(csse_data, 10), 
                        selected = top_n_countries(csse_data, 3))
     
   })
   
   ## Define leaflet plot. Only include Tiles
   ## Other components will be added later.
   output$distPlot <- renderLeaflet({
     leaflet() %>% addTiles() %>%
       setView(lng = 0, lat = 0, zoom = 1)
   })
   
   
   ## Observe Event to redraw circles on leaflet whenever case_type changes
   observeEvent(input$case_type, {
     ## Draw circles for Confirmed Cases
     if(input$case_type == "Confirmed Cases"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date)),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Confirmed.Cases) * 3000,
                    label = ~htmltools::htmlEscape(Confirmed.Cases.Text),
                    color = '#7570b3')
     }
     ## Draw circles for Confirmed Deaths
     else if(input$case_type == "Confirmed Deaths"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date)),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Confirmed.Deaths) * 3000,
                    label = ~htmltools::htmlEscape(Confirmed.Deaths.Text),
                    color = '#d95f02')
     }
     ## Draw circles for Recovered Cases
     else if(input$case_type == "Recovered Cases"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date)),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Recovered.Cases) * 3000,
                    label = ~htmltools::htmlEscape(Recovered.Cases.Text),
                    color = '#1b9e77')
     }
     ## ingoreInit to prevent launch errors
   }, ignoreInit = TRUE)
   
   observeEvent(input$date, {
     ## Draw circles for Confirmed Cases
     if(input$case_type == "Confirmed Cases"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date)),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Confirmed.Cases) * 3000,
                    label = ~htmltools::htmlEscape(Confirmed.Cases.Text),
                    color = '#7570b3')
     }
     ## Draw circles for Confirmed Deaths
     else if(input$case_type == "Confirmed Deaths"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date), Confirmed.Deaths > 0),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Confirmed.Deaths) * 3000,
                    label = ~htmltools::htmlEscape(Confirmed.Deaths.Text),
                    color = '#d95f02')
     }
     ## Draw circles for Recovered Cases
     else if(input$case_type == "Recovered Cases"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date), Recovered.Cases > 0),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Recovered.Cases) * 3000,
                    label = ~htmltools::htmlEscape(Recovered.Cases.Text),
                    color = '#1b9e77')
     }
     ## ingoreInit to prevent launch errors
   }, ignoreInit = TRUE)
   
   ## Define Cumulative Cases Graph
   output$cumulative_graph <- renderPlot({
     if(input$case_type == "Confirmed Cases"){
       ## Filter by selectee countries and date. 
       ## Plot simple line and point plots
       calculate_cum(filter(csse_data, Country.Region %in% input$countries, 
                            cast_date <= ymd(input$date)), "Confirmed.Cases") %>%
         ggplot(aes(cast_date, cum_cases, col = Country.Region)) + 
         geom_line() + geom_point() + 
         labs(x = "Date", y = "Cumulative Confirmed Cases",
              title = "Cumulative Cases by Country", color = "Country") +
         theme_bw()
     }
     else if(input$case_type == "Confirmed Deaths"){
       ## Filter by selectee countries and date. 
       ## Plot simple line and point plots
       calculate_cum(filter(csse_data, Country.Region %in% input$countries, 
                            cast_date <= ymd(input$date)), "Confirmed.Deaths") %>%
         ggplot(aes(cast_date, cum_cases, col = Country.Region)) + 
         geom_line() + geom_point() + 
         labs(x = "Date", y = "Cumulative Deaths",
              title = "Cumulative Deaths by Country", color = "Country") +
         theme_bw()
     }
     else if(input$case_type == "Recovered Cases"){
       ## Filter by selectee countries and date. 
       ## Plot simple line and point plots
       calculate_cum(filter(csse_data, Country.Region %in% input$countries, 
                            cast_date <= ymd(input$date)), "Recovered.Cases") %>%
         ggplot(aes(cast_date, cum_cases, col = Country.Region)) + 
         geom_line() + geom_point() + 
         labs(x = "Date", y = "Cumulative Recovered Cases",
              title = "Cumulative Recovered Cases by Country", color = "Country") +
         theme_bw()
     }
   })
   
   ## Define Data Source reference
   url <- a("Johns Hopkins University Center for Systems Science and Engineering", href = "https://github.com/CSSEGISandData/COVID-19")
   output$reference <- renderUI({
     tagList("Data Source:", url)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

