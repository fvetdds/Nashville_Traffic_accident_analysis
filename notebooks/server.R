

library(shiny)
function(input, output, session) { 
  filteredData <- reactive({
    accidents %>% 
      filter(Date.and.Time >= input$dateRange[1] & Date.and.Time <= input$dateRange[2],
             (input$weather == "All" | Weather.Description == input$weather),
             (input$illumination == "All" | Illumination.Description == input$illumination),
              Hour >= input$timeOfDay[1] & Hour <= input$timeOfDay[2])
    })

  
  # **tab 1 Interactive Crash Map**
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
  output$statChart <- renderPlot({
    if(input$statChartTab2 == "Top 15 Zip codes for accidents") { 
      top_zipcodes <- accidents %>%
        count(Zip.Code, name = "AccidentCount") %>%
        arrange(desc(AccidentCount)) %>%
        head(15) 
       c1<- ggplot(top_zipcodes, aes(x = fct_reorder(Zip.Code, AccidentCount), y = AccidentCount)) +
        geom_bar(stat = "identity", fill = "blue") +
        coord_flip()+
        labs(title = "Top 15 Zip codes for Accidents", x = "Zip code", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24),  
              axis.title.x = element_text(size = 16, margin = margin(t = 20)),
              axis.title.y = element_text(size = 18), 
              axis.text.x = element_text(size = 14), 
              axis.text.y = element_text(size = 14))
    Sortedacccident <- accidents %>%
      filter(Zip.Code == "37013") %>%
      mutate(Year = as.integer(Year))
      c2 <- ggplot(Sortedacccident, aes(x = Year)) +
      geom_bar(stat ="count", fill = "darkred") +
      scale_x_continuous(name = "Year", breaks = seq(min(Sortedacccident$Year, na.rm = TRUE),
                                                     max(Sortedacccident$Year, na.rm = TRUE),
                                                     by = 1)) +
      scale_y_continuous(name = "Number of Accidents") +
      labs(title = "Total Accidents by Year in Zip Code 37013") +
      theme_minimal() +
      theme(plot.title =element_text(size = 24), axis.title.x = element_text(size = 18, margin = margin(t = 20)), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14, hjust = 1), axis.text.y = element_text(size = 14))
    return(grid.arrange(c1, c2, ncol = 1, heights = c(2.0, 1.8)))
      
    } else if (input$statChartTab2 == "How Often Do Traffic Accidents Lead to Injuries?") {
      Injury_count <- accidents %>%
        count(Accident, name = "count") %>%
        mutate(Proportion = count/sum(count) *100) 
      ggplot(Injury_count, aes(x = "", y = count, fill = Accident)) +
        geom_bar(stat = "identity", width = 1) +  
        coord_polar(theta = "y") +  
        labs(title = "Proportion of Accidents with and without Injuries") +
        theme_void() +  
        geom_text(aes(label = paste0(round(Proportion, 1), "%")), 
                  position = position_stack(vjust = 0.5), 
                  size = 6, color = "black") +
        theme(
          plot.title = element_text(size = 28, hjust = 0.5),
          legend.text = element_text(size = 20),  # Increase legend text size
          legend.title = element_text(size = 20)  # Increase legend title size
        ) +
          scale_fill_manual(values = c("Injuries" = "red", "No Injuries" = "gray"),
                          labels = c("Injuries" = "With Injuries", "No Injuries" = "No Injuries")) 
        
    }else if (input$statChartTab2 == "Hit-and-Run Cases: Pattern and Impact") {
      HitRun_count <- accidents %>%
        mutate(Hit.and.Run = ifelse(Hit.and.Run == "", "A", Hit.and.Run)) %>% 
        filter((Hit.and.Run) != "A") %>%
        count(Hit.and.Run, name = "HitCount") %>%
        mutate(HitProportion = HitCount/sum(HitCount)*100)
        c1 <- ggplot(HitRun_count, aes(x ="", y = HitCount, fill = Hit.and.Run)) +
        geom_bar(stat = "identity", width = 2) +
        coord_polar(theta = "y") +
        labs(title = "Proportion of Hit-and-Run cases in Accidents") + 
        theme_void() +
        geom_text(aes(label = paste0(round(HitProportion, 1), "%")),
         position = position_stack(vjust = 0.5),
         size = 6, color = "black") +
        theme(
          plot.title = element_text(size = 28, hjust = 0.5),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20)
        ) +
        scale_fill_manual(values = c("Y" = "darkorange", "N" = "darkgray"),
                          labels = c("Y" = "With Hit and Run", "N" = "No Hit and Run"))
      
      hit_area <- accidents %>%
        filter(Hit.and.Run == "Y") %>%
        count(Zip.Code, name = "ZipCode_count") %>%
        head(10)
        c2 <- ggplot(hit_area, aes(x = fct_reorder(Zip.Code, ZipCode_count), y = ZipCode_count)) +
        geom_bar(stat = "identity", fill = "blue") +
        coord_flip()+
        labs(title = "Top 10 Zip codes for Hit and Run Cases", x = "Zip code", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24),  
              axis.title.x = element_text(size = 16, margin = margin(t = 20)),
              axis.title.y = element_text(size = 18), 
              axis.text.x = element_text(size = 14), 
              axis.text.y = element_text(size = 14)) 
        grid.arrange(c1, c2, ncol = 1, heights = c(2.0, 1.8))
     
    } else if (input$statChartTab2 == "Breakdown of Collision Types in Traffic Accidents") {
      p1 <- ggplot(accidents, aes(x = Collision.Type.Description)) +
      geom_bar(fill = "blue") +
      labs(title = "Distribution of Collision Types in Traffic Accidents", x = "Collision Type", y = "Number of Accidents") +
      theme_minimal() +
      theme(plot.title =element_text(size = 18), axis.title.x = element_text(size = 14, margin = margin(t = 10)), axis.title.y = element_text(size = 14), axis.text.x = element_text(size = 12, angle = 45, hjust = 1), axis.text.y = element_text(size = 12))
      
      img_path <- "www/COLLISION TYPE.png"
      img_grob <- rasterGrob(png::readPNG(img_path), interpolate = TRUE)
      
      return(grid.arrange(p1, img_grob, ncol = 2, heights = c(2.0, 2.0)))
    }
  })
  
   # **Tab 3 Time analysis**
  
  output$selectedtime <- renderPlot({
    if(input$timeSortTab3 == "Monthly Trend") {
      p1<- ggplot(accidents, aes(x = Month)) +
        geom_bar(fill = "blue") +
        labs(title = "Total Accidents per Month", x = "Month", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24), axis.title.x = element_text(size = 18, margin = margin(t = 20)), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14, angle = 45, hjust = 1), axis.text.y = element_text(size = 14))
 
           
      p2 <- ggplot(accidents, aes(x = Season)) +
        geom_bar(fill = "darkred") +
        labs(title = "Seasonal Contribution to Traffic Accidents", x = "Season", y = "Total Accidents") +
        theme_minimal() +
        theme(plot.title = element_text(size = 24, margin = margin(b = 20)), axis.title.x = element_text(size = 18, margin = margin(t = 20)), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
      
      grid.arrange(p1, p2, ncol = 1, heights = c(2.0, 1.8))
    } else if (input$timeSortTab3 == "Day of Week Trend") {
      ggplot(accidents, aes(x = Weekday)) +
        geom_bar(fill = "blue") +
        labs(title = "Total Accidents per Day of Week", x = "Day of Week", y = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24), axis.title.x = element_text(size = 18, margin = margin(t = 20)), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))
    } else if (input$timeSortTab3 == "Hourly Trend") {
      ggplot(accidents, aes(x = Hour)) +
        geom_bar(fill = "darkgreen") +
        labs(title = "Total Accidents per Hour") +
        scale_x_discrete(name = "Time of Day", labels = c("12 AM", "1 AM", "2 AM", "3 AM", "4 AM", "5 AM", "6 AM", "7 AM", "8 AM", "9 AM", "10 AM", "11 AM",
                                                          "12 PM", "1 PM", "2 PM", "3 PM", "4 PM", "5 PM", "6 PM", "7 PM", "8 PM", "9 PM", "10 PM", "11 PM")) +
        scale_y_continuous(name = "Number of Accidents") +
        theme_minimal() +
        theme(plot.title =element_text(size = 24, margin = margin(b = 60)), axis.title.x = element_text(size = 16, margin = margin(t = 20)), axis.title.y = element_text(size = 18), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 14))
    }  
  }) 
  
  
  # ** Tab 4 Raw Data Table** 
    filteredDataTab4 <- reactive({
      accidents %>%
        filter(Date.and.Time >= input$dateRangeTab4[1] & Date.and.Time <= input$dateRangeTab4[2],
               (input$weatherTab4 == "All" | Weather.Description == input$weatherTab4),
               (input$illuminationTab4 == "All" | Illumination.Description == input$illuminationTab4),
               Hour >= input$timeOfDayTab4[1] & Hour <= input$timeOfDayTab4[2])
  })
   output$accidentDataTable <- renderDT({
    datatable(filteredDataTab4(), 
              options = list(pageLength = 10, autoWidth = TRUE))
  })         


  
  output$downloadDataTab5 <- downloadHandler(
    filename = function() { paste0("filtered_Nashville_accidents_", Sys.Date(), ".csv") },
    content = function(file) { write.csv(filteredDataTab5(), file, row.names = FALSE) })
}
