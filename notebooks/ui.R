#
#

library(shiny)

fluidPage(
  tags$head(
    tags$style(HTML(" 
    .custom-title {
      font-size: 36pt;
      font-weight: bold;
      color: #333333;
      text-align: center;
      margin-bottom: 20px;
    }
                    /* Navbar Background - Sky Blue */
                      .navbar-default {
                        background-color: #3498DB !important;
                          border-color: #3498DB;
                      }
                    .navbar-default .navbar-brand, 
                    .navbar-default .navbar-nav > li > a {
                      color: white !important; /* White text for contrast */
                    }
                    
                    /* Active Tab Styling - Golden Yellow */
                      .navbar-default .navbar-nav > .active > a, 
                    .navbar-default .navbar-nav > .active > a:focus, 
                    .navbar-default .navbar-nav > .active > a:hover {
                      background-color: #F39C12 !important;
                        color: white !important;
                    }
                    
                    /* Sidebar Panel - Light Gray */
                      .well {
                        background-color: #F8F9FA !important;
                          color: black !important;
                        border-radius: 8px;
                        padding: 15px;
                      }
                    
                    /* Input Labels - Dark Gray for Readability */
                      label {
                        font-weight: bold;
                        color: #333333 !important;
                      }
                    
                    /* Dropdown Menus */
                      .selectize-input {
                        background-color: white !important;
                        color: black !important;
                      }
                    
                    /* Slider Input - Golden Yellow */
                      .irs-bar {
                        background: #F39C12 !important;
                      }
                    
                    /* Buttons - Teal */
                      .btn {
                        background-color: #17A2B8 !important;
                          color: white !important;
                        border-radius: 5px;
                      }
                    
                    /* Main Panel - Light Gray */
                      .mainPanel {
                        background-color: #F8F9FA;
                          padding: 10px;
                        border-radius: 5px;
                      }
                    "))
  ),
  
  
  # Application title
  div(class = "shiny-title-panel"),
  
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
             tabPanel("Total Injuries and Fatalities",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput(
                            "dateRangeTab2",
                            "Select Date Range:",
                            start = min(accidents$Date.and.Time, na.rm = TRUE),
                            end = max(accidents$Date.and.Time, na.rm = TRUE)
                          ),
                          selectInput(
                            "weatherTab2",
                            "Weather Condition:",
                            choices = c("All", unique(accidents$Weather.Description)),
                            selected = "All"
                          ),
                          selectInput(
                            "illuminationTab2",
                            "Illumination Condition:",
                            choices = c("All", unique(accidents$Illumination.Description)),
                            selected = "All"
                          ),
                          sliderInput(
                            "timeOfDayTab2",
                            "Time of Day (Hour):",
                            min = 0,
                            max = 23,
                            value = c(0, 23),
                            step = 1
                          )
                        ),
                        mainPanel(
                          plotOutput("fatalityInjuryBarChart", height = "500px")
                        )
                      )
             ),
             tabPanel("Accident Hotspots by Zip Code",
                      sidebarLayout(
                        sidebarPanel(
                          dateRangeInput(
                            "dateRangeTab3",
                            "Select Date Range:",
                            start = min(accidents$Date.and.Time, na.rm = TRUE),
                            end = max(accidents$Date.and.Time, na.rm = TRUE)
                          ),
                          selectInput(
                            "weatherTab3",
                            "Weather Condition:",
                            choices = c("All", unique(accidents$Weather.Description)),
                            selected = "All"
                          ),
                          selectInput(
                            "illuminationTab3",
                            "Illumination Condition:",
                            choices = c("All", unique(accidents$Illumination.Description)),
                            selected = "All"
                          ),
                          sliderInput(
                            "timeOfDayTab3",
                            "Time of Day (Hour):",
                            min = 0,
                            max = 23,
                            value = c(0, 23),
                            step = 1
                          )
                        ),
                        mainPanel(
                          plotOutput("accidentByZipChart", height = "500px")
                        )
                      )
             ),
             tabPanel("Accident Statistical data",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "weatherTab4",
                            "Weather Condition:",
                            choices = unique(accidents$Weather.Description),
                            selected = unique(accidents$Weather.Description)[1]
                          ),
                          selectInput(
                            "illuminationTab4",
                            "Illumination Condition:",
                            choices = unique(accidents$Illumination.Description),
                            selected = unique(accidents$Illumination.Description)[1]
                          ),
                          sliderInput(
                            "timeOfDayTab4",
                            "Time of Day (Hour):",
                            min = 0,
                            max = 23,
                            value = 12,
                            step = 1
                          )
                        ),
                        mainPanel(
                          plotOutput("accidentProbabilityChart", height = "500px")
                        )
                   )
           )
  )
)
