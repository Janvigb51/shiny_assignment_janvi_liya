#Import the DIG dataset
#setwd("C:/Users/janvi/Desktop/R Assignment 5/")
dig <- read.csv("DIG-1.csv")
# Install the required packages
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shiny)
library(tidyverse)
library(DynNom)
library(shinydashboard)

# Import the DIG dataset
#setwd("C:/Users/janvi/Desktop/shiny_assignment_janvi_liya/R Assignment 5/")
dig <- read.csv("DIG-1.csv")
# Cleaning the data + selecting desired variables
dig$TRTMT <- factor(dig$TRTMT, levels = c(0,1), labels = c("placebo","treatment"))
dig$SEX <- factor(dig$SEX, levels = c(1,2), labels = c("male","female"))
dig$DEATH <- factor(dig$DEATH, levels = c(0,1), labels = c("death","alive"))
dig$HOSP <- factor(dig$HOSP, levels = c(0,1), labels = c("NO","YES"))
dig$CVD <- factor(dig$CVD, levels = c(0,1), labels = c("NO","YES"))
dig_data <- dig %>%
  select(ID, TRTMT, AGE, SEX, BMI, DIABP, SYSBP, CVD, HOSP, HOSPDAYS, DEATH, DEATHDAY)

# Get Started
# Define User Interface and Server + functions
ui <- dashboardPage(
  dashboardHeader(title = "Digitalis Investigation Group", titleWidth = 450),
  dashboardSidebar(
    radioButtons(inputId = "treatment", label = "Select Treatment:", choices = c("treatment", "placebo")),
    sliderInput(inputId = "age", label = "Select Age:", min = 20, max = 90, value = c(60, 70)),
    radioButtons(inputId = "sex", label = "Select Gender:", choices = c("female", "male")),
    sliderInput(inputId = "bmi", label = "Select Body Mass Index:", min = 10, max = 65, value = c(10, 65)),
    sliderInput(inputId = "diabp", label = "Select Diastolic BP:", min = 20, max = 190, value = c(60, 90)),
    sliderInput(inputId = "sysbp", label = "Select Systolic BP:", min = 70, max = 220, value = c(100, 140))),
    radioButtons(inputId = "cvd", label = "Select Cardiovascular Disease:", choices = c("NO", "YES")),
    radioButtons(inputId = "hosp", label = "Select Hospitalization:", choices = c("NO", "YES")),
    sliderInput(inputId = "hospdays", label = "Select Hospitalization Days:", min = 0, max = 1800, value = c(30, 90)),
    radioButtons(inputId = "death", label = "Select Vital Status:", choices = c("Death", "Alive")),
    sliderInput(inputId = "deathdays", label = "Select Death Day:", min = 0, max = 1800, value = c(1300, 1600)),
    
    dashboardBody(plotOutput("plot1"),
    dataTableOutput("table1")))

server <- function(input, output) {}

shinyApp(ui = ui, server = server)


