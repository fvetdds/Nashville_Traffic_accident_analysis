#
#

library(shiny)
server <- function(input, output, session) {
  filteredData <- reactive({
    data <- accidents %>%
      filter(Date.and.Time >= input$dateRange[1] & Date.and.Time <= input$dateRange[2]) %>%
      mutate(Hour = as.numeric(format(Date.and.Time, "%H"))) %>%
      filter(Hour >= input$timeOfDay[1] & Hour <= input$timeOfDay[2])
    
    if (input$weather != "All") data <- data %>% filter(Weather.Description == input$weather)
    if (input$illumination != "All") data <- data %>% filter(Illumination.Description == input$illumination)
    
    return(data)
  })
  
  # 🔹 Leaflet Map Initial Rendering
  output$accidentMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = mean(accidents$Longitude, na.rm = TRUE), 
              lat = mean(accidents$Latitude, na.rm = TRUE), zoom = 10)
  })
  
  # 🔹 Update Leaflet Map Dynamically
  observe({
    data <- filteredData()
    total_accidents <- formatC(nrow(data), format = "f", big.mark = ",", digits = 0)
    
    leafletProxy("accidentMap", data = data) %>%
      clearMarkers() %>%
      clearControls() %>% 
      addCircleMarkers(
        lng = ~Longitude, lat = ~Latitude,
        color = "#2C3E50", fillColor = "#3498DB",
        radius = 1, fillOpacity = 0.7, stroke = TRUE, group = "Filtered",
        popup = ~paste0("<b>Date:</b> ", Date.and.Time, "<br>",
                        "<b>Injuries:</b> ", Number.of.Injuries, "<br>",
                        "<b>Fatalities:</b> ", Number.of.Fatalities, "<br>",
                        "<b>Collision Type:</b> ", Collision.Type.Description)
      ) %>%
      addControl(
        html = paste0("<h4 style='color: black;'>Total Accidents: <b>", total_accidents, "</b></h4>"),
        position = "topright"
      )
  })
  
  # 🔹 Render Fatality & Injury Bar Chart
  output$fatalityInjuryBarChart <- renderPlot({
    data <- filteredData() %>%
      summarise(
        Total_Fatalities = sum(Number.of.Fatalities, na.rm = TRUE),
        Total_Injuries = sum(Number.of.Injuries, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Type", values_to = "Count")
    
    ggplot(data, aes(x = Type, y = Count, fill = Type)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.7) +
      labs(title = "Total Fatalities and Injuries", x = "Type", y = "Count") +
      theme_minimal()
  })
  
  # 🔹 Render Accidents by ZIP Code
  output$accidentByZipChart <- renderPlot({
    data <- filteredData() %>%
      group_by(Zip.Code) %>%
      summarise(Total_Accidents = n(), .groups = "drop") %>%
      arrange(desc(Total_Accidents))
    
    ggplot(data, aes(x = reorder(as.factor(Zip.Code), Total_Accidents), y = Total_Accidents, fill = Total_Accidents)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      coord_flip() +
      labs(title = "Total Accidents by Zip Code", x = "Zip Code", y = "Total Accidents") +
      theme_minimal()
  })
  
  # 🔹 Render Accident Probability Chart
  output$accidentProbabilityChart <- renderPlot({
    data <- filteredData()
    
    total_accidents <- nrow(accidents)
    filtered_accidents <- nrow(data)
    probability <- (filtered_accidents / total_accidents) * 100
    
    ggplot(data.frame(Condition = c("Filtered", "Total"), Count = c(filtered_accidents, total_accidents)), 
           aes(x = Condition, y = Count, fill = Condition)) +
      geom_bar(stat = "identity", alpha = 0.7) +
      labs(title = sprintf("Accident Probability: %.2f%%", probability), x = "Condition", y = "Number of Accidents") +
      theme_minimal()
  })
}