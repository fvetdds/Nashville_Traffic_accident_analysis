library(shiny)
library(leaflet)
fluidPage(
  tags$head(tags$style(HTML("
      .navbar-default { background-color: #3498DB; border-color: #3498DB; }
      .navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a { color: white; }
      .navbar-default .navbar-nav > .active > a { background-color: #F39C12; color: white; }
      .btn { background-color: #17A2B8; color: white; border-radius: 5px; }
  "))),
  
  # create a navigation bar with 4 tabs and  users can use 4 filters for 1.date range, 2.weather, 3.illumination type and 4.time of the day
  navbarPage("Music City CrashView Application",
             tabPanel("Interactive Crash Map", 
                      fluidRow( 
                        column(3, 
                               wellPanel(
                                 dateRangeInput( 
                                   "dateRange",
                                   "Select Date Range:",
                                   start = as.Date("2024-02-01"),
                                   end = as.Date("2024-02-25"),
                                   min = min(accidents$Date.and.Time, na.rm = TRUE),
                                   max = max(accidents$Date.and.Time, na.rm = TRUE)
                                 ),
                                   
                                 selectInput(
                                   "weather",
                                   "Weather Condition:",
                                   choices = c("All", unique(accidents$Weather.Description)),
                                   selected = "All"
                                 ),
                                 selectInput(
                                   "illumination",
                                   "Illumination Condition:",
                                   choices = c("All", unique(accidents$Illumination.Description)),
                                   selected = "All"
                                 ),
                                 sliderInput(
                                   "timeOfDay",
                                   "Time of Day (Hour):",
                                   min = 0,
                                   max = 23,
                                   value = c(0, 23),
                                   step = 1
                                 ),
                                 
                                 actionButton("searchBT", "Search Accidents")
                               )
                        ),
                        column(9, leafletOutput("accidentMap", height = "700px")
                        )
                      )
             ),
             
             
             
             #Tab 2: Accident Statistical Data
             tabPanel("Accident Statistical Data",
                      fluidRow(
                        column(3,
                               wellPanel(
                                 selectInput("statChartTab2", "Select Chart:", choices = c(
                                   "Top 15 Zip codes for accidents",  
                                   "How Often Do Traffic Accidents Lead to Injuries?", 
                                   "Hit-and-Run Cases: Pattern and Impact", 
                                   "Breakdown of Collision Types in Traffic Accidents",
                                   "When Do Most Accidents Happen? A Look at Weather Trends",
                                   "Brighter Roads, Safer Drives? Analyzing Illumination and Traffic Accidents"
                                 ))
                                 
                               ) 
                              
                            ),
                        
                        column(9,
                               plotOutput("statChart", width = "100%", height = "700px"),
                               uiOutput("pdfViewer"),
                        )
                      )    
             ),
             
             
             
             # Tab 3: Time-based accident Analysis
             tabPanel("Time-Based Accident Analysis",
                      fluidRow(
                        column(3,  
                               wellPanel(
                                 
                                 radioButtons("timeSortTab3", "Select Trend Type:", choices = c("Monthly Trend", "Day of Week Trend", "Hourly Trend")),
                                 uiOutput("dynamic_timeSort")
                               )
                        ),
                        column(9,
                               plotOutput("selectedtime",width = "100%", height = "700px")
                        )
                      )
             ),
             
              
             # Tab 4: Raw Data Table
             tabPanel("Raw Data Table",
                      fluidRow(
                        column(3,  
                               wellPanel(
                                 dateRangeInput("dateRangeTab4", "Date Range:", start = min(accidents$Date.and.Time, na.rm = TRUE),
                                                end = max(accidents$Date.and.Time, na.rm = TRUE)),
                                 selectInput("weatherTab4", "Weather Condition:", choices = c("All", unique(accidents$Weather.Description)), selected = "All"),
                                 selectInput("illuminationTab4", "Illumination Condition:", choices = c("All", unique(accidents$Illumination.Description)), selected = "All"),
                                 sliderInput("timeOfDayTab4","Time of Day (Hour):", min = 0, max = 23, value = c(0, 23), step = 1),
                                 downloadButton("downloadDataTab4", "Download Data")
                               )
                        ),
                        column(9,
                               DTOutput("accidentDataTable")
                        )
                      )
             )
  )
  
)

