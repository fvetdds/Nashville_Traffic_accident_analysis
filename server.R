

library(shiny)
function(input, output, session) { 
  filteredData <- reactive({
    accidents %>% 
      filter(Date.and.Time >= input$dateRange[1] & Date.and.Time <= input$dateRange[2],
             (input$weather == "All" | Weather.Description == input$weather),
             (input$illumination == "All" | Illumination.Description == input$illumination),
              Hour >= input$timeOfDay[1] & Hour <= input$timeOfDay[2])
    })

  
  # 🔹 **Interactive Crash Map**
  output$accidentMap <- renderLeaflet({
    leaflet() %>% 
      addTiles() %>%
      setView(lng = mean(accidents$Longitude, na.rm = TRUE), lat = mean(accidents$Latitude, na.rm = TRUE), zoom = 10)
  })
  
  observe({
    data <- filteredData()
    pal <- colorFactor(
      palette = c("gray", "red"),
      levels = c("No Fatalities", "With Fatalities")
    )
    
    leafletProxy("accidentMap", data = data) %>%
      clearMarkers() %>% 
      clearControls() %>%
      addCircleMarkers(lng = ~Longitude, 
                       lat = ~Latitude, 
                       color = "#2C3E50", 
                       fillColor = ~pal(Fatality_category),
                       radius = ~ifelse(Number.of.Injuries > 0, 4 + (Number.of.Injuries * 1.5), 3),
                       fillOpacity = 0.7, stroke = TRUE, weight = 0.5, group = "Filtered",
                       popup = ~paste("<b>Date:</b>", Date.and.Time, "<br><b>Injuries:</b>", Number.of.Injuries,
                                      "<br><b>Fatalities:</b>", Number.of.Fatalities, "<br><b>Collision Type:</b>",
                                      Collision.Type.Description, "<br><b>Hit&Run:</b>", Hit.and.Run,
                                      "<br><b>Property Damage:</b>", Property.Damage)) %>%
      addControl(paste0("<h4 style='color: black; background: white; padding: 5px; border-radius: 5px;'>",
                        "Total Accidents: <b>", format(nrow(data), big.mark = ","), "</b></h4>"),
                 position = "topright") %>%
      addLegend("bottomright", colors = c("gray", "red"), labels = c("No Fatalities", "With Fatalities"), values = filteredData$Fatality_category, title = "Fatalities in Accident", opacity = 1)
  })
  
  # **Tab2 Accident Statistical Data
  
  
  
  # **Tab 3: Understanding Crash Risks
  

  # **Tab 4 Time analysis**
  output$selectedtime <- renderPlot({
    if(input$timeSortTab4 == "Monthly Trend") {
      ggplot(accidents, aes(x = Month)) +
        geom_bar(fill = "navyblue") +
        labs(title = "Total Accidents per Month", x = "Month", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.text.y = element_text(size = 14))
    } else if (input$timeSortTab4 == "Day of Week Trend") {
      ggplot(accidents, aes(x = Weekday)) +
        geom_bar(fill = "darkred") +
        labs(title = "Total Accidents per Day of Week", x = "Day of Week", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
    } else if (input$timeSortTab4 == "Hourly Trend") {
      ggplot(accidents, aes(x = Hour)) +
        geom_bar(fill = "darkgreen") +
        labs(title = "Total Accidents per Hour") +
        scale_x_discrete(name = "Time of Day", labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
                                                          "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")) +
        scale_y_continuous(name = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24), axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 12), xis.text.y = element_text(size = 14))
    }  
  })
  
  
  # ** Tab 5 Raw Data Table**
    filteredDataTab5 <- reactive({
      accidents %>%
        filter(Date.and.Time >= input$dateRangeTab5[1] & Date.and.Time <= input$dateRangeTab5[2],
               (input$weatherTab5 == "All" | Weather.Description == input$weatherTab5),
               (input$illuminationTab5 == "All" | Illumination.Description == input$illuminationTab5),
               Hour >= input$timeOfDayTab5[1] & Hour <= input$timeOfDayTab5[2])
  })
   output$accidentDataTable <- renderDT({
    datatable(filteredDataTab5(), 
              options = list(pageLength = 10, autoWidth = TRUE))
  })         


  
  output$downloadDataTab5 <- downloadHandler(
    filename = function() { paste0("filtered_Nashville_accidents_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(filteredDataTab5(), file, row.names = FALSE) })
}
