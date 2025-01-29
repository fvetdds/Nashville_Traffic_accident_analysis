library(shiny)
library(tidyverse)
library(glue)
library(sf) 
library(lme4)
library(leaflet)
library(shinythemes)
library(DT)

accidents <- read.csv("./data/Nashville_traffic_accident.csv")

accidents$Property.Damage <- as.logical(accidents$Property.Damage)
accidents$Hit.and.Run <- as.logical(accidents$Hit.and.Run)
accidents$Date.and.Time <- as.POSIXct(accidents$Date.and.Time, format="%m/%d/%Y %H:%M")

  