
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



accidents <- read.csv("C:/Users/vetdd/Documents/NSS_project/Nashville_Traffic_accident_analysis/data/Nashville_traffic_accident.csv")

accidents <- accidents %>%
  mutate(
    Property.Damage = as.logical(Property.Damage),
    Hit.and.Run = as.logical(Hit.and.Run),
    Date.and.Time = as.POSIXct(Date.and.Time, format="%m/%d/%Y %H:%M"),
    Year = format(Date.and.Time, "%Y"),
    Hour = as.integer(format(Date.and.Time, "%H")),
    Weekday = weekdays(Date.and.Time),
    DayType = ifelse(Weekday %in% c("Saturday", "Sunday"), "Weekend", "Weekday"),
    Zip.Code = as.character(Zip.Code)
  )