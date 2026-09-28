# About -------------------------------------------------------------------
# HOSPITAL SURGE CAPACITY MODEL
# PAHO/WHO
# Script: Packages
# Last-mod-date: August 2026
# R 4.5.2

# Library -----------------------------------------------------------------
library(shiny)
library(shinydashboard)
library(markdown)
library(rintrojs)
library(simmer)
library(future.apply)
library(dplyr)
library(ggplot2)
library(plotly)
library(readxl)
library(openxlsx)

# Genera manifest.json
# Para publicar una aplicación Shiny en R desde GitHub, Connect Cloud necesita conocer sus dependencias. 
# Desde la raíz del proyecto ejecuta: rsconnect::writeManifest(appDir = ".")
