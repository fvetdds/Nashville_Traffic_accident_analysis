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
                          )
                        ),
                        # leafletOutput will create an interactive map to visualizes accidents
                        mainPanel(
                          leafletOutput("accidentMap", height = "700px")
                        )
                      )
                    ),


             
             # Tab 2: Accident Statistical Data
             tabPanel("Accident Statistical Data",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("statChart", "Select Chart:", choices = c(
                            "Top Zip code for accidents", "Top Preclincts for accidents", 
                            "How Often Do Traffic Accidents Lead to Injuries?", 
                            "Fatal Accidents: Trends and Impact Across Locations", 
                            "Hit-and-Run Cases: Pattern and Impact", 
                            "Impactful Events in Traffic Crashes: A Data Overview", 
                            "Breakdown of Collision Types in Traffic Accidents"
                          )),
                          sliderInput("numZipCodes", "Number of Zip Codes to Show:", min = 1, max = 10, value = 5)
                        ),
                        mainPanel(plotOutput("selectedChart", height = "500px"))
                      )
             ),
             
             # Tab 3: Understanding Crash Risks
             tabPanel("Understanding Crash Risks",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("riskFactorTab3", "Select Factor:", choices = c("Weather", "Illumination", "Hour"))
                        ),
                        mainPanel(
                          conditionalPanel(condition = "input.riskFactor == 'weather'",plotOutput("weatherImpact", height = "500px")),
                          conditionalPanel(condition = "input.riskFactor == 'illumination'",plotOutput("illuminationImpact", height = "500px")),
                          conditionalPanel(condition = "input.riskFactor == 'Hour'",plotOutput("HourImpact", height = "500px"))
                        )
                      )
             ),
             
             # Tab 4: Trend & Time Analysis
             tabPanel("Trend & Time Analysis",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("timeSort", "Select Trend Type:", choices = c("Yearly Trend", "Daily Trend", "Hourly Trend"))
                        ),
                        mainPanel(
                          conditionalPanel(condition = "input.timeSort == 'Yearly Trend'", plotOutput("yearlyTrend", height = "500px")),
                          conditionalPanel(condition = "input.timeSort == 'Daily Trend'", plotOutput("dailyTrend", height = "500px")),
                          conditionalPanel(condition = "input.timeSort == 'Hourly Trend'", plotOutput("hourlyTrend", height = "500px"))
                        )
                      )
             ),
             
             # Tab 5: Raw Data Table
             tabPanel("Raw Data Table",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput("dateRangeTab5", "Date Range:", start = min(accidents$Date.and.Time, na.rm = TRUE),
                                         end = max(accidents$Date.and.Time, na.rm = TRUE)),
                          selectInput("weatherTab5", "Weather Condition:", choices = c("All", unique(accidents$Weather.Description)), selected = "All"),
                          selectInput("illuminationTab5", "Illumination Condition:", choices = c("All", unique(accidents$Illumination.Description)), selected = "All"),
                          sliderInput("timeOfDayTab5","Time of Day (Hour):", min = 0, max = 23, value = c(0, 23), step = 1),
                          downloadButton("downloadDataTab5", "Download Data")
                        ),
                        mainPanel(DTOutput("accidentDataTable"))
                      )
             )
  )
)


