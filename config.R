#Config file: toy Shiny dashboard with no senstive data
#Passwords are for demonstration purposes only

#Load packages
library(shiny)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(DT)
library(datasets)
library(ggplot2)
library(ggthemes)

#Passwords
users <- c("test", "dev")
passwords <- c("test", "test")