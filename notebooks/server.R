#
#

library(shiny)
server <- function(input, output) {
  filteredData <- reactive({
    data <- accidents %>%
      filter(
        Date.and.Time >= input$dateRange[1] &
          Date.and.Time <= input$dateRange[2]
      )
    
    # Extract hour from Date.and.Time and filter by timeOfDay
    data <- data %>% mutate(Hour = as.numeric(format(Date.and.Time, "%H")))
    data <- data %>% filter(Hour >= input$timeOfDay[1] & Hour <= input$timeOfDay[2])
    
    if (input$weather != "All") {
      data <- data %>% filter(Weather.Description == input$weather)
    }
    
    if (input$illumination != "All") {
      data <- data %>% filter(Illumination.Description == input$illumination)
    }
    
    data
  })
  
  filteredDataTab2 <- reactive({
    data <- accidents %>%
      filter(
        Date.and.Time >= input$dateRangeTab2[1] &
          Date.and.Time <= input$dateRangeTab2[2]
      )
    
    # Extract hour from Date.and.Time and filter by timeOfDay
    data <- data %>% mutate(Hour = as.numeric(format(Date.and.Time, "%H")))
    data <- data %>% filter(Hour >= input$timeOfDayTab2[1] & Hour <= input$timeOfDayTab2[2])
    
    if (input$weatherTab2 != "All") {
      data <- data %>% filter(Weather.Description == input$weatherTab2)
    }
    
    if (input$illuminationTab2 != "All") {
      data <- data %>% filter(Illumination.Description == input$illuminationTab2)
    }
    
    data
  })
  
  filteredDataTab3 <- reactive({
    data <- accidents %>%
      filter(
        Date.and.Time >= input$dateRangeTab3[1] &
          Date.and.Time <= input$dateRangeTab3[2]
      )
    
    # Extract hour from Date.and.Time and filter by timeOfDay
    data <- data %>% mutate(Hour = as.numeric(format(Date.and.Time, "%H")))
    data <- data %>% filter(Hour >= input$timeOfDayTab3[1] & Hour <= input$timeOfDayTab3[2])
    
    if (input$weatherTab3 != "All") {
      data <- data %>% filter(Weather.Description == input$weatherTab3)
    }
    
    if (input$illuminationTab3 != "All") {
      data <- data %>% filter(Illumination.Description == input$illuminationTab3)
    }
    
    data
  })
  
  filteredDataTab5 <- reactive({
    data <- accidents %>%
      filter(
        Weather.Description == input$weatherTab5 &
          Illumination.Description == input$illuminationTab5 &
          as.numeric(format(Date.and.Time, "%H")) == input$timeOfDayTab5
      )
    
    data
  })
  
  output$accidentMap <- renderLeaflet({
    data <- filteredData()
    
    leaflet(data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~paste(
          "Date:", Date.and.Time, "<br>",
          "Injuries:", Number.of.Injuries, "<br>",
          "Fatalities:", Number.of.Fatalities, "<br>",
          "Collision Type:", Collision.Type.Description
        ),
        radius = 5,
        color = "red",
        fill = TRUE,
        fillOpacity = 0.5
      )
  })
  
  output$fatalityInjuryBarChart <- renderPlot({
    data <- filteredDataTab2()
    
    summary_data <- data %>%
      summarise(
        Total_Fatalities = sum(Number.of.Fatalities, na.rm = TRUE),
        Total_Injuries = sum(Number.of.Injuries, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(Total_Fatalities, Total_Injuries), names_to = "Type", values_to = "Count")
    
    ggplot(summary_data, aes(x = Type, y = Count, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(
        title = "Total Fatalities and Injuries",
        x = "Type",
        y = "Count",
        fill = "Type"
      ) +
      theme_minimal()
  })
  
  output$accidentByZipChart <- renderPlot({
    data <- filteredDataTab3()
    
    summary_data <- data %>%
      group_by(Zip.Code) %>%
      summarise(Total_Accidents = n(), .groups = 'drop') %>%
      arrange(desc(Total_Accidents))
    
    ggplot(summary_data, aes(x = reorder(as.factor(Zip.Code), Total_Accidents), y = Total_Accidents, fill = Total_Accidents)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      coord_flip() +
      labs(
        title = "Total Accidents by Zip Code",
        x = "Zip Code",
        y = "Total Accidents",
        fill = "Accidents"
      ) +
      theme_minimal() 
  })
  
  output$accidentProbabilityChart <- renderPlot({
    data <- filteredDataTab5()
    
    total_accidents <- nrow(accidents)
    filtered_accidents <- nrow(data)
    probability <- (filtered_accidents / total_accidents) * 100
    
    ggplot(data.frame(Condition = c("Filtered", "Total"), Count = c(filtered_accidents, total_accidents)), aes(x = Condition, y = Count, fill = Condition)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(
        title = sprintf("Accident Probability: %.2f%%", probability),
        x = "Condition",
        y = "Number of Accidents",
        fill = "Condition"
      ) +
      theme_minimal()
  })
}

