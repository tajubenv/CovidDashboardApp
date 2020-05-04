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
library(shinyWidgets) ## Used for drop down checkboxes
library(rgeos) ## Used for getting lat and long
library(rworldmap) ## Used for getting lat and long

calculate_cum <- function(data, field){
  ## Calculate cumulative cases for data cleaning
  output <- data %>% filter(field > 0) %>% 
    mutate(date_char = as.character(cast_date)) %>% 
    group_by(Country.Region, date_char) %>% 
    summarise(cum_cases = sum(get(field), na.rm = T)) %>% 
    mutate(cast_date = ymd(date_char)) %>% 
    rename(!!field := cum_cases)
  
  
  return(output)
}

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
  
  ## NEW SECTION FOR BETTER DATA CLEANING
  confirmed_cases_csse_gather <- calculate_cum(confirmed_cases_csse_gather, "Confirmed.Cases")
  
  confirmed_deaths_csse_gather <- calculate_cum(confirmed_deaths_csse_gather, "Confirmed.Deaths")
  
  confirmed_recovered_csse_gather <- calculate_cum(confirmed_recovered_csse_gather, "Recovered.Cases")
  
  ## Join all data together and removes 0 rows for confirmed cases
  csse_complete <- left_join(confirmed_cases_csse_gather, 
                             confirmed_deaths_csse_gather, 
                             by = c('Country.Region', 'cast_date')) %>% 
    left_join(confirmed_recovered_csse_gather, by = c('Country.Region',
                                                      'cast_date')) %>% 
    filter(Confirmed.Cases > 0) 
  
 
  ## Create Character Variables for marker output later
  csse_complete <- csse_complete %>% 
    mutate(Confirmed.Cases.Text = paste0("Confirmed Cases: ", Confirmed.Cases),
           Confirmed.Deaths.Text = paste0("Confirmed Deaths: ", Confirmed.Deaths),
           Recovered.Cases.Text = paste0("Recovered Cases: ", Recovered.Cases))
  
  ## Add central coordinates
  wmap <- getMap(resolution = "high")
  centroids <- gCentroid(wmap, byid=TRUE)
  
  ## Manual renaming of mismatch between coordinates
  ## This is needed as data is now summarized for each country,
  ## so coord data must be taken from one source
  df <- as.data.frame(centroids) %>% 
    rename(Lat = y, Long = x) %>% 
    rownames_to_column("Country.Region")  %>%
    mutate(Country.Region = case_when(Country.Region == "The Bahamas"  ~ "Bahamas",
                                      Country.Region == "Cape Verde"  ~ "Cabo Verde",
                                      Country.Region == "South Korea" ~ "Korea, South",
                                      Country.Region == "United States of America" ~ "US",
                                      Country.Region == "West Bank" ~ "West Bank and Gaza",
                                      TRUE ~ Country.Region)) 
  
  ## Use contiguous coordinates for US
  us_contig_coord <- c("US", -98.5795, 39.8282)
  
  df[df$Country.Region == "US",] <- us_contig_coord
  
  ## Ensure all coordinates are numeric
  df$Long <- as.numeric(df$Long)
  df$Lat <- as.numeric(df$Lat)
  
  ## Combine coordinates with covid data
  csse_complete <- left_join(csse_complete, df, by = "Country.Region")
  
  return(csse_complete)
}

top_n_countries <- function(data, n){
  ## Retreive top n countries for covid19 cases from csse data
  top_n <-data %>% 
    group_by(Country.Region) %>% 
    summarize(max_cases = max(Confirmed.Cases)) %>% 
    arrange(desc(max_cases)) %>% head(n) %>% 
    pull(Country.Region)
  return(top_n)
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
         uiOutput("country_selector"),
         ## Text describing app
         uiOutput("app_description")
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
   
   ##
   output$app_description <- renderUI({
     title <- tags$b(htmltools::htmlEscape("Description:"))
     line1 <- htmltools::htmlEscape("This app visualizes the current state of
                                     COVID-19 prevalence based on data that is
                                     aggregated by John Hopkins CSSE.")
     line2 <- htmltools::htmlEscape("The date range may be selected using the slider above.
                                     The graph uses the range, while the map shows only the end date.")
     line3 <- htmltools::htmlEscape("The case types include: Confirmed Cases, which are cases with a 
                                     positive test result. Confirmed Deaths, which are deaths officially
                                     associated with COVID-19. Recovered Cases are confirmed cases that
                                     are considered COVID-19 free. These definitions may vary based on the 
                                     institution that is tracking them.")
     line4 <- htmltools::htmlEscape("The country selector determines which countries will be shown
                                     on the cumulative graph.")
     line5 <- htmltools::htmlEscape("This data should not be used for medical guidance or even
                                     as an official count of COVID-19 prevalence.")
     HTML(paste(title, line1, line2, line3, line4, line5, sep = '<br/>'))
   })
   ##  Define Date Slider 
   output$date_slider <- renderUI({
     sliderInput("date",
                 "Date:",
                 min = ymd(min(csse_data$cast_date)),
                 max = ymd(max(csse_data$cast_date)),
                 value = c(ymd(min(csse_data$cast_date)),
                           ymd(max(csse_data$cast_date)))
     )
   })
   
   ## Define country selector based on countries with top 5 cases
   ## All countries may now be selected
   ## Updated to use pickerInput from shinyWidgets
   output$country_selector <- renderUI({
     pickerInput("countries",
                 "Cases Plot Countries:",
                 choices = as.character(unique(csse_data$Country.Region)), 
                 selected = top_n_countries(csse_data, 5),
                 options = list(`actions-box` = TRUE),
                 multiple = TRUE)
     
   })
   
   ## Define leaflet plot. Only include Tiles
   ## Other components will be added later.
   ## Addition of options and Max Bounds limits
   ## the scrollable area of the map.
   output$distPlot <- renderLeaflet({
     leaflet() %>% 
       addTiles(options = providerTileOptions(minZoom = 1.3, 
                                              maxZoom = 10)) %>%
       setView(lng = 0, lat = 0, zoom = 2) %>%
       setMaxBounds(lng1 = -180, lng2 = 180,
                    lat1 = -90, lat2 = 90)
   })
   
   
   ## Observe Event to redraw circles on leaflet whenever case_type changes
   observeEvent(c(input$case_type, input$date), {
     ## Draw circles for Confirmed Cases
     if(input$case_type == "Confirmed Cases"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date[2]), !is.na(Lat)),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Confirmed.Cases) * 2000,
                    label = ~htmltools::htmlEscape(Confirmed.Cases.Text),
                    color = '#7570b3')
     }
     ## Draw circles for Confirmed Deaths
     else if(input$case_type == "Confirmed Deaths"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date[2]), !is.na(Lat)),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Confirmed.Deaths) * 2000,
                    label = ~htmltools::htmlEscape(Confirmed.Deaths.Text),
                    color = '#d95f02')
     }
     ## Draw circles for Recovered Cases
     else if(input$case_type == "Recovered Cases"){
       leafletProxy("distPlot")   %>%
         clearShapes() %>%
         addCircles(data = filter(csse_data, cast_date == ymd(input$date[2]), !is.na(Lat)),
                    lng = ~Long, lat = ~Lat,
                    radius = ~sqrt(Recovered.Cases) * 2000,
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
       filter(csse_data, Country.Region %in% input$countries, 
                            cast_date <= ymd(input$date[2]),
                            cast_date >= ymd(input$date[1]),
              Confirmed.Cases > 0) %>%
         ggplot(aes(cast_date, Confirmed.Cases, col = Country.Region)) + 
         geom_line() + geom_point() + 
         labs(x = "Date", y = "Cumulative Confirmed Cases",
              title = "Cumulative Cases by Country", color = "Country") +
         theme_bw()
     }
     else if(input$case_type == "Confirmed Deaths"){
       ## Filter by selectee countries and date. 
       ## Plot simple line and point plots
       filter(csse_data, Country.Region %in% input$countries, 
              cast_date <= ymd(input$date[2]),
              cast_date >= ymd(input$date[1])) %>%
         ggplot(aes(cast_date, Confirmed.Deaths, col = Country.Region)) + 
         geom_line() + geom_point() + 
         labs(x = "Date", y = "Cumulative Deaths",
              title = "Cumulative Deaths by Country", color = "Country") +
         theme_bw()
     }
     else if(input$case_type == "Recovered Cases"){
       ## Filter by selectee countries and date. 
       ## Plot simple line and point plots
       filter(csse_data, Country.Region %in% input$countries, 
              cast_date <= ymd(input$date[2]),
              cast_date >= ymd(input$date[1])) %>%
         ggplot(aes(cast_date, Recovered.Cases, col = Country.Region)) + 
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

