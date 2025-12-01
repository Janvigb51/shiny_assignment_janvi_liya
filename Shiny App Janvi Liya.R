#Import the DIG dataset
setwd("C:/Users/janvi/Desktop/R Assignment 5/")
dig <- read.csv("DIG-1.csv")
#Install the required packages
if (!require("shiny")) install.packages("shiny")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("DynNom")) install.packages("DynNom")
if (!require("shinydashboard")) install.packages("shinydashboard")
library(shiny)
library(tidyverse)
library(DynNom)
library(shinydashboard)
