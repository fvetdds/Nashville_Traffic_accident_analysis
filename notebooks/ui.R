#
#

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
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput( 
                            "dateRange",
                            "Select Date Range:",
                            start = min(accidents$Date.and.Time, na.rm = TRUE),
                            end = max(accidents$Date.and.Time, na.rm = TRUE)
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
                        ),
                        # leafletOutput will create an interactive map to visualizes accidents
                        mainPanel(
                          leafletOutput("accidentMap", height = "700px")
                        )
                      )
                    ),


             
              #Tab 2: Accident Statistical Data
             tabPanel("Accident Statistical Data",
                      sidebarLayout(
                        sidebarPanel(
                         selectInput("statChartTab2", "Select Chart:", choices = c(
                            "Top 15 Zip codes for accidents",  
                            "How Often Do Traffic Accidents Lead to Injuries?", 
                            "Hit-and-Run Cases: Pattern and Impact", 
                            "Breakdown of Collision Types in Traffic Accidents",
                            "When Do Most Accidents Happen? A Look at Weather Trends",
                            "Brighter Roads, Safer Drives? Analyzing Illumination and Traffic Accidents"
                          )),
                            uiOutput("dynamicstatTab2")
                         ),
                                                  
                        mainPanel(
                          plotOutput("statChart", height = "800px"),
                          
                        )
                      )    
                    ),
             
             
             
             # Tab 3: Time-based accident Analysis
             tabPanel("Time-Based Accident Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("timeSortTab3", "Select Trend Type:", choices = c("Monthly Trend", "Day of Week Trend", "Hourly Trend")),
                          uiOutput("dynamic_timeSort")
                        ),
                        mainPanel(
                          plotOutput("selectedtime", height = "800px"))
                      )
             ),
                      
             
             # Tab 4: Raw Data Table
             tabPanel("Raw Data Table",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("dateRangeTab4", "Date Range:", start = min(accidents$Date.and.Time, na.rm = TRUE),
                                         end = max(accidents$Date.and.Time, na.rm = TRUE)),
                          selectInput("weatherTab4", "Weather Condition:", choices = c("All", unique(accidents$Weather.Description)), selected = "All"),
                          selectInput("illuminationTab4", "Illumination Condition:", choices = c("All", unique(accidents$Illumination.Description)), selected = "All"),
                          sliderInput("timeOfDayTab4","Time of Day (Hour):", min = 0, max = 23, value = c(0, 23), step = 1),
                          downloadButton("downloadDataTab4", "Download Data")
                        ),
                        mainPanel(DTOutput("accidentDataTable")
                        )
                      )
             )
        )

)

