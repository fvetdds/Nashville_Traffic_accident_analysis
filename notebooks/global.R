
library(shiny)
library(tidyverse)  
library(leaflet)
library(shinythemes) 
library(DT)
library(scales)
library(rsconnect)
library(grid)
library(gridExtra)
library(plotly)






accidents <- readRDS("www/accidents.rds") 


accidents <- accidents %>% 
  mutate(
    Property.Damage = as.logical(Property.Damage),
    Hit.and.Run = as.character(Hit.and.Run),
    Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M"),
    
    Year = format(Date.and.Time, "%Y"),
    Month = factor(format(Date.and.Time, "%B"), levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
    Season = case_when(Month %in% c("March", "April", "May") ~ "Spring",
                       Month %in% c("June", "July", "August") ~ "Summer",
                       Month %in% c("September", "October", "November") ~ "Fall",
                       Month %in% c("December", "January", "February") ~ "Winter", TRUE ~ "Unknown"),
    Hour = format(Date.and.Time, "%H"),
    Weekday = factor(format(Date.and.Time, "%A"), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    DayType = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
    Zip.Code = as.character(Zip.Code),
    Accident = ifelse(Number.of.Injuries>0, "Injuries", "No Injuries")
  ) %>%
  filter(!is.na(Month) & !is.na(Weekday) & !is.na(Hour)) 

accidents <- accidents %>%
  mutate (Fatality_category = ifelse(Number.of.Fatalities == 0, "No Fatalities", "With Fatalities"))

year_accidents <- accidents %>%
  filter(Year != 2025) %>%
  group_by(Year) %>%
  summarise(Total_Accidents = n(), Total_Injuries = sum(Number.of.Injuries)) %>%
  arrange(Year)
