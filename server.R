

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
  
  # 🔹 **Accident Statistical Data (Multiple Plots)**
  output$accidentStatsChart <- renderPlot({
    data <- filteredData()
    stats <- list(
      ggplot(data %>% count(Year), aes(Year, n)) +
        geom_bar(stat = "identity", fill = "royalblue") +
        labs(title = "Total Accidents per Year", x = "Year", y = "Total Accidents") + theme_minimal(),
      
      ggplot(data %>% count(DayType), aes(DayType, n, fill = DayType)) +
        geom_bar(stat = "identity") +
        labs(title = "Weekday vs. Weekend Accidents", x = "Day Type", y = "Total Accidents") + theme_minimal(),
      
      ggplot(data %>% count(Weekday) %>% top_n(5, n), aes(reorder(Weekday, -n), n)) +
        geom_bar(stat = "identity", fill = "purple") +
        labs(title = "Busiest Days for Accidents", x = "Weekday", y = "Total Accidents") + theme_minimal(),
      
      ggplot(data %>% count(Hour) %>% top_n(5, n), aes(Hour, n)) +
        geom_bar(stat = "identity", fill = "red") +
        labs(title = "Busiest Hours for Accidents", x = "Hour", y = "Total Accidents") + theme_minimal(),
      
      ggplot(data %>% count(Zip.Code) %>% top_n(5, n), aes(reorder(as.factor(Zip.Code), -n), n)) +
        geom_bar(stat = "identity", fill = "brown") +
        labs(title = "Top Zip Codes with Most Accidents", x = "Zip Code", y = "Total Accidents") + theme_minimal()
    )
    grid.arrange(grobs = stats, ncol = 2)
  })
  
  # 🔹 **Accident Probability Logistic Regression**
  
  filteredDataTab3 <- reactive({
    accidents %>%
      filter((input$weather == "All" | Weather == input$weather),
             (input$illumination == "All" | Illumination == input$illumination),
             Hour >= input$timeOfDay[1] & Hour <= input$timeOfDay[2])
  })
  
  # 🔹 **Probability of Each Weather Condition Causing an Accident**
  output$weatherImpact <- renderPlot({
    if (input$riskFactorTab3 == "Weather") {
      data <- filteredDataTab3()
      model <- glm(Accident ~ Weather, data = data, family = binomial)
      
      probabilities <- model %>%
        tidy() %>%
        filter(term != "(Intercept)") %>%
        mutate(
          Weather = sub("Weather", "", term),
          Probability = plogis(estimate)
        )
      
      ggplot(probabilities, aes(x = reorder(Weather, Probability), y = Probability, fill = Probability)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Probability of an Accident by Weather Condition",
             x = "Weather Condition", y = "Probability of Accident") +
        theme_minimal()
    }
  })
  
  # 🔹 **Probability of Illumination Condition Causing an Accident**
  output$illuminationImpact <- renderPlot({
    if (input$riskFactorTab3 == "Illumination") {
      data <- filteredDataTab3()
      model <- glm(Accident ~ Illumination, data = data, family = binomial)
      
      probabilities <- model %>%
        tidy() %>%
        filter(term != "(Intercept)") %>%
        mutate(
          Illumination = sub("Illumination", "", term),
          Probability = plogis(estimate)
        )
      
      ggplot(probabilities, aes(x = reorder(Illumination, Probability), y = Probability, fill = Probability)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = "Probability of an Accident by Illumination Condition",
             x = "Illumination Condition", y = "Probability of Accident") +
        theme_minimal()
    }
  })
  
  # 🔹 **Probability of Hourly Accident Occurrence**
  output$HourImpact <- renderPlot({
    if (input$riskFactorTab3 == "Hour") {
      data <- filteredDataTab3()
      model <- glm(Accident ~ Hour, data = data, family = binomial)
      
      pred_data <- data %>%
        group_by(Hour) %>%
        summarize(Accident_Prob = mean(predict(model, newdata = ., type = "response")), .groups = "drop")
      
      ggplot(pred_data, aes(x = Hour, y = Accident_Prob)) +
        geom_line(color = "red", size = 1) +
        geom_point(color = "blue") +
        labs(title = "Predicted Probability of an Accident by Hour",
             x = "Time of Day (Hour)", y = "Probability of an Accident") +
        theme_minimal()
    }
  })
  
  # 🔹 **Trend & Time Analysis**
  output$Trend <- renderPlot({
    data <- filteredDataTab3() %>%
      count(Date = as.Date(Date.and.Time))
    
    ggplot(data, aes(x = Date, y = n)) +
      geom_line(color = "blue") + geom_point(color = "red") +
      labs(title = "Accident Trends Over Time", x = "Date", y = "Total Accidents") +
      theme_minimal()
  })
  
}  

  # 🔹 **Trend & Time Analysis**
  output$Trend <- renderPlot({
    data <- filteredData() %>% count(Date = as.Date(Date.and.Time))
    
    ggplot(data, aes(x = Date, y = n)) +
      geom_line(color = "blue") + geom_point(color = "red") +
      labs(title = "Accident Trends Over Time", x = "Year", y = "Total Accidents") +
      theme_minimal()
    
  }   
)
  #**Raw Data Table**
    filteredDataTab5 <- reactive({
      accidents %>%
        filter(Date.and.Time >= input$dateRangeTab5[1] & Date.and.Time <= input$dateRangeTab5[2],
               (input$weatherTab5 == "All" | Weather.Description == input$weatherTab5),
               (input$illuminationTab5 == "All" | Illumination.Description == input$illuminationTab5),
               Hour >= input$timeOfDayTab5[1] & Hour <= input$timeOfDayTab5[2])
  })
  output$accidentDataTable <- renderDT({
    datatable(filteredDataTab5(), options = list(pageLength = 20, autoWidth = TRUE))
  })
  
  output$downloadDataTab5 <- downloadHandler(
    filename = function() { paste0("filtered_accidents_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(filteredDataTab5(), file, row.names = FALSE) }
    )

