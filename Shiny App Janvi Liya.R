#Import the DIG dataset
#setwd("C:/Users/janvi/Desktop/R Assignment 5/")
dig <- read.csv("DIG-1.csv")
# Install the required packages
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")

if (!require("plotly")) install.packages("plotly")

library(shiny)
library(tidyverse)
library(DynNom)
library(shinydashboard)
library(shinyWidgets)

library(plotly)

# Import the DIG dataset
#setwd("C:/Users/janvi/Desktop/shiny_assignment_janvi_liya/R Assignment 5/")
dig <- read.csv("DIG-1.csv")
# Cleaning the data + selecting desired variables
dig$TRTMT <- factor(dig$TRTMT, levels = c(0,1), labels = c("placebo","treatment"))
dig$SEX <- factor(dig$SEX, levels = c(1,2), labels = c("male","female"))
dig$DEATH <- factor(dig$DEATH, levels = c(1,0), labels = c("death","alive"))
dig$HOSP <- factor(dig$HOSP, levels = c(0,1), labels = c("NO","YES"))
dig$CVD <- factor(dig$CVD, levels = c(0,1), labels = c("NO","YES"))
dig_data <- dig %>%
  select(ID, TRTMT, AGE, SEX, BMI, DIABP, SYSBP, CVD, HOSP, HOSPDAYS, DEATH, DEATHDAY)

# Get Started
# Define User Interface and Server + functions
ui <- dashboardPage(
  dashboardHeader(title = "DIG - Digoxin Data Explorer", titleWidth = 450),
  dashboardSidebar(
    width = 240,
    
  sidebarMenu(
      id = "sidebar",
      style = "position: relative; overflow: visible;",
      menuItem( "Digitalis Investigation Group", tabName = "intro",
                icon = icon("user-md"), selected = TRUE),
      menuItem("Age vs Body Mass Index", tabName = "age_bmi", icon = icon("vial")),
      menuItem(HTML("Diastolic vs Systolic<br>Blood Pressure"),
               tabName = "bp", icon = icon("heartbeat")),
      menuItem("All together", tabName = "parallel", icon = icon("chart-pie")),
      conditionalPanel(
        condition = "input.sidebar == 'age_bmi'",
        checkboxGroupInput(inputId = "treatment", label = "Select Treatment:",
                           choices = c("treatment", "placebo"),
                           selected = c("treatment", "placebo")),
        checkboxGroupInput(inputId = "sex", label = "Select Gender:",
                           choices = c("male", "female"),
                           selected = c("male", "female")),
        sliderInput(inputId = "age", label = "Select Age:",
                    min = 20, max = 90, value = c(60, 70)),
        sliderInput(inputId = "bmi", label = "Select BMI (Body Mass Index):",
                    
                  min = 10, max = 65, value = c(10, 65))),
      conditionalPanel(
        
      condition = "input.sidebar == 'bp'",
        checkboxGroupInput(inputId = "treatment", label = "Select Treatment:",
                           
                          choices = c("treatment", "placebo"),
                           selected = c("treatment", "placebo")),
        checkboxGroupInput(inputId = "sex", label = "Select Gender:",
                           choices = c("male", "female"),
                           
                          selected = c("male", "female")),
        sliderInput(inputId = "diabp", label = "Select Diastolic BP:",
                    min = 20, max = 190, value = c(60, 90)),
        sliderInput(inputId = "sysbp", label = "Select Systolic BP:",
                    min = 70, max = 220, value = c(100, 140))),
      conditionalPanel(
        condition = "input.sidebar == 'parallel'",
        downloadButton("downloadParallel", "Download Plot"))),

#    dropdownButton(
#      inputId = "filters", label = "Show filters", icon = icon("sliders"), circle = FALSE, status = "primary", width = "320px",
#      checkboxGroupInput(inputId = "treatment", label = "Select Treatment:", choices = c("treatment", "placebo"), selected = c("treatment", "placebo")),
#      sliderInput(inputId = "age", label = "Select Age:", min = 20, max = 90, value = c(60, 70)),
#      checkboxGroupInput(inputId = "sex", label = "Select Gender:", choices = c("male", "female"), selected = c("male", "female")),
#      sliderInput(inputId = "bmi", label = "Select BMI (Body Mass Index):", min = 10, max = 65, value = c(10, 65)),
#      sliderInput(inputId = "diabp", label = "Select Diastolic BP:", min = 20, max = 190, value = c(60, 90)),
#      sliderInput(inputId = "sysbp", label = "Select Systolic BP:", min = 70, max = 220, value = c(100, 140)),
#      checkboxGroupInput(inputId = "cvd", label = "Select Cardiovascular Disease:", choices = c("NO", "YES"), selected = c("NO", "YES")),
#      checkboxGroupInput(inputId = "hosp", label = "Select Hospitalization:", choices = c("NO", "YES"), selected = c("NO", "YES")),
#      sliderInput(inputId = "hospdays", label = "Select Hospitalization Days:", min = 0, max = 1800, value = c(30, 90)),
#      checkboxGroupInput(inputId = "death", label = "Select Vital Status:", choices = c("death", "alive"), selected = c("death", "alive")),
#      sliderInput(inputId = "deathdays", label = "Select Death Day:", min = 0, max = 1800, value = c(1300, 1600)))),


  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h3("Welcome"),
              br(),
              p("This dashboard allows you to explore the DIG (Digitalis Investigation Group)
                dataset. Digoxin, derived from the Digitalis plant, is a medication
                that has been used for years to help strenghten heart contractions 
                and control heart rate. The purpose of the this clinical trial was
                to examine the safety and efficacy of Digoxin in treating patients
                with congestive heart failure in sinus rhythm."),
              br(),
              div(style = "margin-bottom: 25px;",
                  img(src = "digoxin.png", width = "80%"),
                  div(p("Image source: ",
                        em(a("The American Journal of Medicine Blog",
                             href = "https://amjmed.org/contemporary-role-for-digoxin-in-heart-failure/",
                             target = "_blank"))),
                      
                    style = "bottom: 1px;
                      right: 10px;
                      font-size: 15px;
                      padding: 2px 4px;
                      
                    border-radius: 3px")),
              p("To start with, navigate to the Age vs BMI tab in the left to explore some relationships within the dataset."),
              br(),
              p("Enjoy the journey,"),
              
            p("Janvi and Liya.")),
      tabItem(tabName = "age_bmi",
              plotOutput("plot1")),
      tabItem(tabName = "bp",
              
            plotOutput("plot2")),
      tabItem(tabName = "parallel",
              plotOutput("parallelplot"))


    server <- function(input, output, session) {
  observeEvent(input$sidebar, {
    if (input$sidebar == "age_bmi") {
      updateCheckboxGroupInput(session, "treatment", selected = character(0))
      updateCheckboxGroupInput(session, "sex", selected = character(0))
    }
    if (input$sidebar == "bp") {
      updateCheckboxGroupInput(session, "treatment", selected = character(0))
      updateCheckboxGroupInput(session, "sex", selected = character(0))
    }
  })
  dig_1 <- reactive({
    req(input$sidebar == "age_bmi")
    df <- dig_data
    if (length(input$treatment) == 0 || length(input$sex) == 0) {
      return(df[0, ])}
    dig_data %>%
      filter(TRTMT %in% input$treatment) %>%
      filter(AGE  >= input$age[1],  AGE  <= input$age[2]) %>%
      filter(SEX %in% input$sex) %>%
      filter(BMI  >= input$bmi[1],  BMI  <= input$bmi[2])
  })
  output$plot1 <- renderPlot({
    req(input$sidebar == "age_bmi")
    validate(
      need(length(input$treatment) > 0,
           "Please select at least one treatment group"),
      need(length(input$sex) > 0,
           "Please select at least one gender"))
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
  dig_2 <- reactive({
    req(input$sidebar == "bp")
    df <- dig_data
    if (length(input$treatment) == 0 || length(input$sex) == 0) {
      return(df[0, ])}
    dig_data %>%
      filter(TRTMT %in% input$treatment) %>%
      filter(SEX %in% input$sex) %>%
      filter(DIABP  >= input$diabp[1],  DIABP  <= input$diabp[2]) %>%
      filter(SYSBP >= input$sysbp[1], SYSBP <= input$sysbp[2])
  })
  output$plot2 <- renderPlot({
    req(input$sidebar == "bp")
    validate(
      need(length(input$treatment) > 0,
           "Please select at least one treatment group"),
      need(length(input$sex) > 0,
           "Please select at least one gender"))
    df_2 <- dig_2()
    ggplot(data = df_2, mapping = aes(x = SYSBP, y = DIABP, colour = TRTMT, shape = SEX)) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = c("placebo" = "darkblue", "treatment" = "maroon")) +
      scale_shape_manual(values = c("male" = 15, "female" = 17)) +
      labs(title = "Diastolic vs Systolic BP",
           colour = "Treatment (by color)",
           shape = "Sex (by shape)",
           x = "Systolic BP",
           y= "Diastolic BP")
})}

dig_parallel <- reactive({
  req(input$sidebar == "parallel")
  dig_data %>%
    na.omit() %>%
    mutate(
      TRTMT_num = as.numeric(TRTMT) - 1,
      SEX_num = as.numeric(SEX) - 1,
      CVD_num = as.numeric(CVD) - 1,
      HOSP_num = as.numeric(HOSP) - 1,
      DEATH_num = as.numeric(DEATH) - 1
    )
})

  output$parallelPlot <- renderPlotly({
    df_parallel <- dig_parallel()
    plot_ly(
      type = 'parcoords',
      line = list(color = df_parallel$TRTMT_num,
                  colorscale = list(list(0, 'hotpink'), list(1, 'green4'))),
      dimensions = list(
        list(tickvals = c(0, 1), ticktext = c("placebo", "Treatment"),
             label = 'Treatment', values = df_parallel$TRTMT_num),
        list(range = c(min(df_parallel$AGE), max(df_parallel$AGE)),
             label = 'Age', values = df_parallel$AGE),
        list(tickvals = c(0, 1), ticktext = c("Male", "Female"),
             label = 'Sex', values = df_parallel$SEX_num),
        list(range = c(min(df_parallel$BMI), max(df_parallel$BMI)),
             label = 'BMI', values = df_parallel$BMI),
        list(range = c(min(df_parallel$DIABP), max(df_parallel$DIABP)),
             label = 'Diastolic BP', values = df_parallel$DIABP),
        list(range = c(min(df_parallel$SYSBP), max(df_parallel$SYSBP)),
             label = 'Systolic BP', values = df_parallel$SYSBP),
        list(tickvals = c(0,1), ticktext = c("NO", "YES"),
             label = 'CVD', values = df_parallel$CVD_num),
        list(tickvals = c(0,1), ticktext = c("NO", "YES"),
             label = 'Hosp', values = df_parallel$HOSP_num),
        list(range = c(min(df_parallel$HOSPDAYS), max(df_parallel$HOSPDAYS)),
             label = 'Hosp days', values = df_parallel$HOSPDAYS),
        list(tickvals = c(0,1), ticktext = c("death", "alive"),
             label = 'Vital Status', values = df_parallel$DEATH_num),
        list(range = c(min(df_parallel$DEATHDAY), max(df_parallel$DEATHDAY)),
             label = 'Death day', values = df_parallel$DEATHDAY)
      )
    )
  })
  

output$downloadParallel <- downloadHandler(
    filename = function() {
      "parallel_plot.html"
    },
    content = function(file) {
      htmlwidgets::saveWidget(as_widget(output$parallelPlot()), file)
    })

shinyApp(ui = ui, server = server)

## Some progress: made 2 graphs
