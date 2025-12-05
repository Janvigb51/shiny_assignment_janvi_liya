#Import the DIG dataset
#setwd("C:/Users/janvi/Desktop/R Assignment 5/")
dig <- read.csv("DIG-1.csv")

# Install the required packages
if (!require("janitor")) install.packages("janitor")
if (!require("lubridate")) install.packages("lubridate")
if (!require("table1")) install.packages("table1")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
if (!require("htmlwidgets")) install.packages("htmlwidgets")
if (!require("plotly")) install.packages("plotly")
if (!require("DT")) install.packages("DT")
library(janitor)
library(lubridate)
library(table1)
library(ggplot2)
library(shiny)
library(tidyverse)
library(DynNom)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(htmlwidgets)
library(plotly)
library(DT)

# Cleaning the data + selecting desired variables
dig_data <- dig %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, DIABETES,
         HYPERTEN, CVD, WHF, HOSP, HOSPDAYS, DEATH, DEATHDAY) %>%
  na.omit()%>%
  mutate(
    TRTMT = factor(TRTMT, levels = c(0,1), labels = c("placebo","treatment")),
    SEX = factor(SEX, levels = c(1,2), labels = c("male","female")),
    DIABETES = factor(DIABETES, levels = c(0,1), labels = c("NO", "YES")),
    HYPERTEN = factor(HYPERTEN, levels = c(0,1), labels = c("NO", "YES")),
    CVD = factor(CVD, levels = c(0,1), labels = c("NO","YES")),
    WHF = factor(WHF, levels = c(0,1), labels = c("NO", "YES")),
    HOSP = factor(HOSP, levels = c(0,1), labels = c("NO","YES")),
    DEATH = factor(DEATH, levels = c(0,1), labels = c("alive","death")))

# Get Started
# Define User Interface and Server + functions
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "DIG (Digoxin) Data Explorer", titleWidth = 450),
  dashboardSidebar(
    width = 240,
    sidebarMenu(
      id = "sidebar",
      style = "position: relative; overflow: visible;",
      menuItem( "Digitalis Investigation Group", tabName = "intro",
                icon = icon("user-md"), selected = TRUE),
      menuItem("THE DATASET", tabName = "dataset", icon = icon("folder-open")),
      menuItem("Age vs Body Mass Index", tabName = "age_bmi", icon = icon("vial")),
      menuItem(HTML("Diastolic vs Systolic<br>Blood Pressure"),
               tabName = "bp", icon = icon("heartbeat")),
      menuItem(HTML("Age Distribution<br>by Vital Status"),
               tabName = "age_death", icon = icon("hourglass-half")),
      menuItem(HTML("Hospital Days<br>by Vital Status"),
               tabName = "hosp_death", icon = icon("syringe")),
      menuItem(HTML("Death Probability<br>Across Age"),
               tabName = "death_prob", icon = icon("chart-line")),
      menuItem("All together", tabName = "parallel", icon = icon("chart-pie")),
      conditionalPanel(
        condition = "input.sidebar == 'dataset'",
        downloadButton("downloadData", label = "Download Dataset")),
      conditionalPanel(
        condition = "input.sidebar == 'age_bmi'",
        checkboxGroupInput(inputId = "treatment", label = "Select Treatment:",
                           choices = c("treatment", "placebo"),
                           selected = c("treatment", "placebo")),
        checkboxGroupInput(inputId = "sex", label = "Select Gender:",
                           choices = c("male", "female"),
                           selected = c("male", "female")),
        sliderInput(inputId = "age", label = "Select Age Range:",
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
        condition = "input.sidebar == 'age_death'",
        sliderInput(inputId = "age", label = "Select Age Range:",
                    min = 20, max = 90, value = c(20, 50))),
      conditionalPanel(
        condition = "input.sidebar == 'hosp_death'",
        sliderInput(inputId = "hosp", label = "Select Hospital Days Range:",
                    min = 0, max = 2000, value = c(0, 1500))),
      conditionalPanel(
        condition = "input.sidebar == 'death_prob'",
        sliderInput(inputId = "age_window", label = "Age (press the play button below)",
                    min = 20, max = 90, value = 30, step = 1, animate = animationOptions(interval = 300))),
      conditionalPanel(
        condition = "input.sidebar == 'parallel'",
        downloadButton("downloadParallel", label = "Download Plot")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "intro",
              h3("Welcome"),
              br(),
              p("This dashboard allows you to explore the DIG (Digitalis Investigation Group)
                Trial dataset. Digoxin, derived from the Digitalis plant, is a medication
                that has been used for years to help strenghten heart contractions 
                and control heart rate. The purpose of the this clinical trial was
                to examine the safety and efficacy of Digoxin in treating patients
                with congestive heart failure in sinus rhythm."),
              br(),
              div(style = "margin-bottom: 25px;",
                  img(src = "digoxin.png", width = "50%"),
                  div(p("Image source: ",
                        em(a("The American Journal of Medicine Blog",
                             href = "https://amjmed.org/contemporary-role-for-digoxin-in-heart-failure/",
                             target = "_blank"))),
                    style = "bottom: 1px;
                      right: 10px;
                      font-size: 15px;
                      padding: 2px 4px;
                      border-radius: 3px")),
              p("To start with, navigate to the Age vs BMI tab in the left panel
                to explore some relationships and how certain variables vary 
                within the dataset."),
              br(),
              p("Enjoy the journey"),
              p("Janvi and Liya.")),
      tabItem(tabName = "dataset",
              fluidRow(
                box(
                  title = "DIG Dataset",
                  width = 8,
                  status = "primary",
                  solidHeader = TRUE,
                  div(
                    style = "overflow-x: auto; width: 100%",
                    DT::dataTableOutput("dataset_table"))),
                box(
                  title = "Abbreviatons",
                  width = 4,
                  status = "info",
                  solidHeader = TRUE,
                  tableOutput("abbrev_table")))),
      tabItem(tabName = "age_bmi",
              plotlyOutput("plot1", height = "600px", width = "90%")),
      tabItem(tabName = "bp",
              plotlyOutput("plot2", height = "600px", width = "90%")),
      tabItem(tabName = "age_death",
              plotlyOutput("plot3", height = "600px", width = "80%"),
              br(),
              p("Here, you can view the distribution of age among patients who
                remained alive or died after follow-up. The colors represent the
                different treatment groups. Click on one of the options from the
                legend at the top right corner to hide that particular group.
                Select a particular region in the plot to zoom in, double-click
                anywhere in the plot to zoom out.")),
      tabItem(tabName = "hosp_death",
              plotlyOutput("plot4", height = "600px", width = "80%"),
              br(),
              p("These histograms show the distribution of hospital stay duration
                for alive and deceased patients, with patiens separated by placebo/
                treatment groups. Click on one of the options from the legend at
                the top right corner to hide that particular group. Select a
                particular region in either plot to zoom in, double-click anywhere
                in that plot to zoom out.")),
      tabItem(tabName = "death_prob",
              plotlyOutput("plot5", height = "600px", width = "80%"),
              br(),
              p("This graphs shows how the probability of death varies as patients
                age, in both the placebo and treatment groups. Click on the",
                em('autoscale'), "icon at the top of the graph to reset the view and
                display the full graph."),
              p("Below, you can see the table showing the probabilities calculated as
                the number of deaths divided by the total patients for each age and
                treatment group. Click on any of the variables (column names) to
                order the data from lowest to highest or viceversa."),
              br(),
              h4("Death Probability Table"),
              dataTableOutput("death_prob_table")),
      tabItem(tabName = "parallel",
              plotlyOutput("parallelplot")))))

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
  # Showing table for the dataset ----------------------------------------------------------------
  datasetWidget <- reactive({
    DT::datatable(dig_data,
                  options = list(
                    scrollx = TRUE,
                    pageLength = 10,
                    autoWidth = TRUE))})
  output$dataset_table <- DT::renderDataTable({datasetWidget()})
  output$abbrev_table <- renderTable({
    data.frame(
      Abbreviation = c("ID","TRTMT","AGE","SEX","BMI","KLEVEL","CREAT",
                   "DIABP","SYSBP","DIABETES","HYPERTEN","CVD","WHF","HOSP",
                   "HOSPDAYS","DEATH","DEATHDAY"),
      Meaning = c("Patient Identification", "Treatment group (Placebo/Treatment)",
                  "Age of patient (years)", "Sex/Gender of patient", "Body Mass Index",
                  "Potassium level", "Creatinine", "Diastolic blood pressure",
                  "Systolic blood pressure", "Diabetes", "Hypertension",
                  "Cardiovascular disease", "Worsening heart failure",
                  "Hospitalization", "Hospital Days", "Vital Status", "Date of death"))
  })
  output$downloadData <- downloadHandler(
    filename = function() {"dataset_table.html"},
    content  = function(file) {
      htmlwidgets::saveWidget(
        widget = datasetWidget(),
        file   = file,
        selfcontained = TRUE)})
  # Plotting age vs bmi -----------------------------------------------------------------------
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
  output$plot1 <- plotly::renderPlotly({
    req(input$sidebar == "age_bmi")
    validate(
      need(length(input$treatment) > 0,
           "Please select at least one treatment group"),
      need(length(input$sex) > 0,
           "Please select at least one gender"))
    df <- dig_1()
    p1 <- ggplot(data = df, mapping = aes(x = AGE, y = BMI, colour = TRTMT, shape = SEX)) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = c("placebo" = "darkblue", "treatment" = "maroon")) +
      scale_shape_manual(values = c("male" = 15, "female" = 17)) +
      labs(title = "Age vs BMI",
           colour = "Treatment (by color)",
           shape = "Sex (by shape)",
           x = "Age",
           y= "BMI")
    plotly::ggplotly(p1, tooltip = c("AGE", "BMI", "TRTMT", "SEX"))
  })
  # Plotting diabp vs sysbp -----------------------------------------------------------------------
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
  output$plot2 <- plotly::renderPlotly({
    req(input$sidebar == "bp")
    validate(
      need(length(input$treatment) > 0,
           "Please select at least one treatment group"),
      need(length(input$sex) > 0,
           "Please select at least one gender"))
    df_2 <- dig_2()
    p2 <- ggplot(data = df_2, mapping = aes(x = SYSBP, y = DIABP, colour = TRTMT, shape = SEX)) +
      geom_point(size = 2, alpha = 0.7) +
      scale_color_manual(values = c("placebo" = "darkblue", "treatment" = "maroon")) +
      scale_shape_manual(values = c("male" = 15, "female" = 17)) +
      labs(title = "Diastolic vs Systolic BP",
           colour = "Treatment (by color)",
           shape = "Sex (by shape)",
           x = "Systolic BP",
           y= "Diastolic BP")
    plotly::ggplotly(p2, tooltip = c("SYSBP", "DIABP", "TRTMT", "SEX"))
  })
  # Plotting age vs vital status -----------------------------------------------------------------------
  dig_3 <- reactive({
    req(input$sidebar == "age_death")
    dig_data %>%
      filter(AGE  >= input$age[1],  AGE  <= input$age[2])
  })
  output$plot3 <- renderPlotly({
    req(input$sidebar == "age_death")
    df_3 <- dig_3()
    plot_ly(data = df_3, x = ~DEATH, y = ~AGE,
           color = ~TRTMT, colors = c("placebo" = "darkblue", "treatment" = "pink2"),
           type = "violin", split = ~DEATH,
           box = list(visible = TRUE), meanline = list(visible = TRUE),
           points = "suspectedoutliers", jitter = 0.1, scalemode = "count")
  })
  # Plotting hsopdays vs vital status -----------------------------------------------------------------------
  dig_4 <- reactive({
    req(input$sidebar == "hosp_death")
    dig_data %>%
      filter(HOSPDAYS >= input$hosp[1], HOSPDAYS <= input$hosp[2])
  })
  output$plot4 <- renderPlotly({
    req(input$sidebar == "hosp_death")
    df_4 <- dig_4()
    p_alive <- plot_ly(data = df_4[df_4$DEATH == "alive", ],
                       x = ~HOSPDAYS, color = ~TRTMT,
                       colors = c("placebo" = "darkblue", "treatment" = "maroon"),
                       type = "histogram", opacity = 0.8) %>%
      layout(barmode = "group", title = "Hospital Days",
             xaxis = list(title = "Alive"),
             yaxis = list(title = "Number of patients"),
             showlegend = TRUE)
    p_death <- plot_ly(data = df_4[df_4$DEATH == "death", ],
                       x = ~HOSPDAYS, color = ~TRTMT,
                       colors = c("placebo" = "darkblue", "treatment" = "maroon"),
                       type = "histogram", opacity = 0.8) %>%
      layout(barmode = "group", title = "Hospital Days",
             xaxis = list(title = "Death"),
             yaxis = list(title = "Number of patients"),
             showlegend = FALSE)
    subplot(p_alive, p_death, nrows = 1, shareY = TRUE, titleX = TRUE, titleY = TRUE) %>%
      layout(showlegend = TRUE)
  })
  # Plotting death prob across age ----------------------------------------------------------------------- 
  # New subset of the data for death probability across age (grouped by treatment)
  death_prob <- dig_data %>%
    group_by(AGE, TRTMT) %>%
    summarise(TOTAL = n(),
              DEATHS = sum(DEATH == "death"),
              PROB = DEATHS / TOTAL) %>%
    ungroup()
  output$plot5 <- renderPlotly({
    req(input$sidebar == "death_prob")
    req(input$age_window)
    # Define some parameters for the moving window (animation)
    window_width <- 15
    center <- input$age_window #the value of age every time it changes (in the middle)
    # For the left side of the window, chose the highest of 2 values:
    # Between smallest age value from the dataset AND 7.5 values to the left of the middle age
    # If (center - window_width/2) happens to be smaller, it picks the smallest age value instead
    x_min <- max(min(death_prob$AGE), center - window_width / 2)
    # For the right side of the window, chose the lowest of 2 values:
    # Between highest age value from the dataset AND 7.5 values to the right of the middle age
    # If (center + window_width/2) happens to be larger, it picks the highest age value instead
    x_max <- min(max(death_prob$AGE), center + window_width / 2)
    plot_ly(data = death_prob,
            x = ~AGE,
            y = ~PROB,
            color = ~TRTMT,
            colors = c("placebo" = "darkblue", "treatment" = "maroon"),
            type = "scatter",
            mode = "lines+markers",
            line = list(width = 2),
            marker = list(size = 3)) %>%
      layout(title = "Probability of Death across Age",
             xaxis = list(title = "Age", range = c(x_min, x_max)),
             yaxis = list(title = "Probability of Death"),
             legend = list(title = list(text = "Treatment Group")))
  })
  output$death_prob_table <- renderDataTable({
    death_prob
  })
  # Plotting parallel -----------------------------------------------------------------------
  dig_parallel <- reactive({
    req(input$sidebar == "parallel")
    dig_data %>%
      na.omit() %>%
      mutate(
        TRTMT_num = as.numeric(TRTMT) - 1,
        SEX_num = as.numeric(SEX) - 1,
        CVD_num = as.numeric(CVD) - 1,
        HOSP_num = as.numeric(HOSP) - 1,
        DEATH_num = as.numeric(DEATH) - 1)
  })
  parallelplot <- reactive({
    df_parallel <- dig_parallel()
    plot_ly(
      type = 'parcoords',
      line = list(color = df_parallel$TRTMT_num,
                  colorscale = list(c(0, "#FF1493"), c(1, "#32CD32"))), 
      dimensions = list(
        list(tickvals = c(0, 1), ticktext = c("Placebo", "Treatment"),
             label = 'Treatment', values = df_parallel$TRTMT_num),
        list(range = c(min(df_parallel$AGE), max(df_parallel$AGE)),
             label = 'Age', values = df_parallel$AGE),
        list(tickvals = c(0,1), ticktext = c("Male", "Female"),
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
        list(tickvals = c(1,0), ticktext = c("death", "alive"),
             label = 'Vital Status', values = df_parallel$DEATH_num),
        list(range = c(min(df_parallel$DEATHDAY), max(df_parallel$DEATHDAY)),
             label = 'Death day', values = df_parallel$DEATHDAY)
      ))
  })
  output$parallelplot <- renderPlotly({
    parallelplot()
  })
  output$downloadParallel <- downloadHandler(
    filename = function() {"parallel_plot.html"},
    content = function(file) {
      htmlwidgets::saveWidget(
        widget = parallelplot(),
        file = file,
        selfcontained = TRUE)})
  
} #server part enclosed

shinyApp(ui = ui, server = server)
