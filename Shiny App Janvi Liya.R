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
dig_data <- dig %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)


# Get Started
# Define User Interface and Server + functions
ui <- dashboardPage(
  dashboardHeader(title = "Digitalis Investigation Group", titleWidth = 450),
  dashboardSidebar(
    radioButtons(inputId = "treatment", label = "Select Treatment:", choices = c("treatment", "placebo")),
    sliderInput(inputId = "age", label = "Select Age:", min = 20, max = 90, value = c(60, 70)),
    radioButtons(inputId = "sex", label = "Select Gender:", choices = c("female", "male")),
    sliderInput(inputId = "bmi", label = "Select Body Mass Index:", min = 10, max = 65, value = c(10, 65)),
    sliderInput(inputId = "klevel", label = "Select Serum Potassium Level:", min = 0, max = 450, value = c(0, 50)),
    sliderInput(inputId = "creatinine", label = "Select Serum Creatinine:", min = 0, max = 3.8, value = c(0.8, 1.4)), 
    sliderInput(inputId = "diabp", label = "Select Diastolic BP:", min = 20, max = 190, value = c(60, 90)),
    sliderInput(inputId = "sysbp", label = "Select Systolic BP:", min = 70, max = 220, value = c(100, 140))),
  
  dashboardBody(
    plotOutput("plot1"),
    dataTableOutput("table1")
  ),)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)


