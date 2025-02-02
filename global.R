
library(shiny)
library(tidyverse)
library(glue)
library(sf) 
library(lme4)
library(leaflet)
library(shinythemes) 
library(DT)
library(scales)
library(rsconnect)
library(broom) 
library(grid)
library(gridExtra)




accidents <- read.csv("C:/Users/vetdd/Documents/NSS_project/Nashville_Traffic_accident_analysis/data/Nashville_traffic_accident.csv")

accidents <- accidents %>%
  mutate(
    Property.Damage = as.logical(Property.Damage),
    Hit.and.Run = as.logical(Hit.and.Run),
    Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M"),
    Year = format(Date.and.Time, "%Y"),
    Month = factor(format(Date.and.Time, "%B"), levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
    Season = case_when(Month %in% c("March", "April", "May") ~ "Spring",
                       Month %in% c("June", "July", "August") ~ "Summer",
                       Month %in% c("September", "October", "November") ~ "Fall",
                       Month %in% c("December", "January", "February") ~ "Winter", TRUE ~ "Unknown"),
    HourAM = format(Date.and.Time, "%I %p"),
    Hour = format(Date.and.Time, "%H"),
    Weekday = factor(weekdays(Date.and.Time), levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
    DayType = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
    Zip.Code = as.character(Zip.Code),
    Accident = ifelse(Number.of.Injuries>0, "Injuries", "No Injuries")
      ) %>%
  filter(!is.na(Month) & !is.na(Weekday) & !is.na(Hour)) 

accidents <- accidents %>%
  mutate (Fatality_category = ifelse(Number.of.Fatalities == 0, "No Fatalities", "With Fatalities"))
          
