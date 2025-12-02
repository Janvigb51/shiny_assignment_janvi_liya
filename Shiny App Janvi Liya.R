#Import the DIG dataset
#setwd("C:/Users/janvi/Desktop/R Assignment 5/")
dig <- read.csv("DIG-1.csv")
# Install the required packages
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
library(shiny)
library(tidyverse)
library(DynNom)
library(shinydashboard)
library(shinyWidgets)

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
    dropdownButton(
      inputId = "filters", label = "Show filters", icon = icon("sliders"), circle = FALSE, status = "primary", width = "320px",
      checkboxGroupInput(inputId = "treatment", label = "Select Treatment:", choices = c("treatment", "placebo"), selected = c("treatment", "placebo")),
      sliderInput(inputId = "age", label = "Select Age:", min = 20, max = 90, value = c(60, 70)),
      checkboxGroupInput(inputId = "sex", label = "Select Gender:", choices = c("male", "female"), selected = c("male", "female")),
      sliderInput(inputId = "bmi", label = "Select BMI (Body Mass Index):", min = 10, max = 65, value = c(10, 65)),
      sliderInput(inputId = "diabp", label = "Select Diastolic BP:", min = 20, max = 190, value = c(60, 90)),
      sliderInput(inputId = "sysbp", label = "Select Systolic BP:", min = 70, max = 220, value = c(100, 140)),
      checkboxGroupInput(inputId = "cvd", label = "Select Cardiovascular Disease:", choices = c("NO", "YES"), selected = c("NO", "YES")),
      checkboxGroupInput(inputId = "hosp", label = "Select Hospitalization:", choices = c("NO", "YES"), selected = c("NO", "YES")),
      sliderInput(inputId = "hospdays", label = "Select Hospitalization Days:", min = 0, max = 1800, value = c(30, 90)),
      checkboxGroupInput(inputId = "death", label = "Select Vital Status:", choices = c("death", "alive"), selected = c("death", "alive")),
      sliderInput(inputId = "deathdays", label = "Select Death Day:", min = 0, max = 1800, value = c(1300, 1600)))),
  dashboardBody(
    plotOutput("plot1"),
  ))

server <- function(input, output) {
  dig_1 <- reactive({
    dig_data %>%
      filter(TRTMT %in% input$treatment) %>%
      filter(AGE  >= input$age[1],  AGE  <= input$age[2]) %>%
      filter(SEX %in% input$sex) %>%
      filter(BMI  >= input$bmi[1],  BMI  <= input$bmi[2]) %>%
      filter(DIABP>= input$diabp[1],DIABP<= input$diabp[2]) %>%
      filter(SYSBP>= input$sysbp[1],SYSBP<= input$sysbp[2]) %>%
      filter(CVD %in% input$cvd) %>%
      filter(HOSP %in% input$hosp) %>%
      filter(DEATH %in% input$death)
  })
  output$plot1 <- renderPlot({
    df <- dig_1()
    ggplot(data = df, mapping = aes(x = AGE, y = BMI, colour = TRTMT, shape = SEX)) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = c("placebo" = "darkblue", "treatment" = "maroon")) +
      scale_shape_manual(values = c("male" = 15, "female" = 17)) +
      labs(title = "Age vs BMI",
           colour = "Treatment (by color)",
           shape = "Sex (by shape)",
           x = "Age",
           y= "BMI")
  })
}

shinyApp(ui = ui, server = server)

## Still needs a lot of editing!
