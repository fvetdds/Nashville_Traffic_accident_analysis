#
#

library(shiny)

# Define UI for application that draws a histogram


fluidPage(
  
  # Application title
  titlePanel("Music City CrashView"),
  
  # Sidebar with inputs for date range and collision type
  navbarPage("Traffic Accident Analyzer",
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
             tabPanel("Accident Probability Prediction",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(
                            "weatherTab5",
                            "Weather Condition:",
                            choices = unique(accidents$Weather.Description),
                            selected = unique(accidents$Weather.Description)[1]
                          ),
                          selectInput(
                            "illuminationTab5",
                            "Illumination Condition:",
                            choices = unique(accidents$Illumination.Description),
                            selected = unique(accidents$Illumination.Description)[1]
                          ),
                          sliderInput(
                            "timeOfDayTab5",
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
